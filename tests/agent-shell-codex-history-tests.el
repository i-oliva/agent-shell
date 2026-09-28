;;; agent-shell-codex-history-tests.el --- History regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-codex-history)

(defun agent-shell-history-test--page (text cursor)
  "Make a page containing TEXT and the next CURSOR."
  (list '(version . 1) (cons 'nextCursor cursor)
        (cons 'updates (vector (list '(sessionUpdate . "agent_message_chunk")
                                     (cons 'content (list '(type . "text")
                                                          (cons 'text text))))))))

(ert-deftest agent-shell-history-empty-metadata-test ()
  "A load without metadata can request a page without mutating its request."
  (with-temp-buffer
    (setq-local agent-shell--state
                (agent-shell--make-state :buffer (current-buffer)
                                         :agent-config '((:identifier . codex))))
    (let* ((agent-shell-codex-history-page-size 40)
           (request (acp-make-session-load-request :session-id "s1" :cwd "/tmp"))
           (original (copy-tree request))
           (page (agent-shell-history-test--page "recent" "older"))
           (response (list (cons '_meta (list (cons agent-shell-codex-history--meta-key page)))))
           (called nil))
      (cl-letf (((symbol-function 'agent-shell-codex-history--insert-link) #'ignore))
        (agent-shell-codex-history--send-request-a
         (lambda (&rest args)
           (should (= 40 (map-nested-elt (plist-get args :request)
                                          '(:params _meta agent-shell.dev/history limit))))
           (funcall (plist-get args :on-success) response))
         :state agent-shell--state :request request
         :on-success (lambda (value) (setq called (eq value response)))))
      (should called)
      (should (equal request original))
      (should (eq agent-shell-codex-history--page page))
      (should (= 1 (length (agent-shell--pop-pending-restore agent-shell--state)))))))

(ert-deftest agent-shell-history-unmodified-adapter-fallback-test ()
  "An adapter ignoring the extension retains its normal history replay."
  (with-temp-buffer
    (setq-local agent-shell--state
                (agent-shell--make-state :buffer (current-buffer)
                                         :agent-config '((:identifier . codex))))
    (let ((agent-shell-codex-history-page-size 40)
          (called nil))
      (map-put! agent-shell--state :pending-restore (agent-shell--make-pending-restore))
      (agent-shell--append-restore-notification
       agent-shell--state (agent-shell-codex-history--notification
                          "s1" '((sessionUpdate . "agent_message_chunk")
                                 (content (type . "text") (text . "full")))))
      (agent-shell-codex-history--send-request-a
       (lambda (&rest args) (funcall (plist-get args :on-success) '((models))))
       :state agent-shell--state
       :request (acp-make-session-load-request :session-id "s1" :cwd "/tmp")
       :on-success (lambda (_) (setq called t)))
      (should called)
      (should-not agent-shell-codex-history--page)
      (should (= 1 (length (agent-shell--pop-pending-restore agent-shell--state)))))))

(defun agent-shell-history-test--reader (body)
  "Call BODY with a source buffer while a history reader is current."
  (let ((source (generate-new-buffer " *history-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "conversation\nCodex> unsent draft")
            (setq-local agent-shell--state (agent-shell--make-state :buffer source))
            (map-put! (map-elt agent-shell--state :session) :id "s1"))
          (with-temp-buffer
            (agent-shell-codex-history-mode)
            (setq agent-shell-codex-history--source source
                  agent-shell-codex-history--session-id "s1"
                  agent-shell-codex-history--pages
                  (list (agent-shell-history-test--page "recent" "older")))
            (agent-shell-codex-history--render-page)
            (funcall body source)))
      (kill-buffer source))))

(ert-deftest agent-shell-history-navigation-preserves-live-buffer-test ()
  "Paging, cached navigation, and end-of-history leave the live buffer alone."
  (agent-shell-history-test--reader
   (lambda (source)
     (let ((before (with-current-buffer source (buffer-string)))
           (state (buffer-local-value 'agent-shell--state source))
           (calls 0))
       (cl-letf (((symbol-function 'acp-send-request)
                  (lambda (&rest args)
                    (setq calls (1+ calls))
                    (should (equal "_agent-shell/history"
                                   (map-elt (plist-get args :request) :method)))
                    (funcall (plist-get args :on-success)
                             (agent-shell-history-test--page "older text" nil)))))
         (agent-shell-codex-history-older)
         (should (= agent-shell-codex-history--index 1))
         (should (string-match-p "older text" (buffer-string)))
         (agent-shell-codex-history-newer)
         (should (string-match-p "recent" (buffer-string)))
         (agent-shell-codex-history-older)
         (should (= calls 1))
         (should-error (agent-shell-codex-history-older) :type 'user-error))
       (should (equal before (with-current-buffer source (buffer-string))))
       (should (eq state (buffer-local-value 'agent-shell--state source)))))))

(ert-deftest agent-shell-history-errors-allow-retry-test ()
  "Both immediate and asynchronous transport failures leave paging retryable."
  (dolist (synchronous '(nil t))
    (agent-shell-history-test--reader
     (lambda (_source)
       (let ((calls 0))
         (cl-letf (((symbol-function 'acp-send-request)
                    (lambda (&rest args)
                      (setq calls (1+ calls))
                      (if (= calls 1)
                          (if synchronous (error "disconnected")
                            (funcall (plist-get args :on-failure) '((message . "disconnected"))))
                        (funcall (plist-get args :on-success)
                                 (agent-shell-history-test--page "retry worked" nil))))))
           (agent-shell-codex-history-older)
           (should-not agent-shell-codex-history--loading)
           (should (string-match-p "disconnected" agent-shell-codex-history--error))
           (should (= agent-shell-codex-history--index 0))
           (agent-shell-codex-history-older)
           (should-not agent-shell-codex-history--error)
           (should (= agent-shell-codex-history--index 1))))))))

(ert-deftest agent-shell-history-rejects-stale-session-response-test ()
  "A response after the source session changes must not enter the reader."
  (agent-shell-history-test--reader
   (lambda (source)
     (let (callback)
       (cl-letf (((symbol-function 'acp-send-request)
                  (lambda (&rest args) (setq callback (plist-get args :on-success)))))
         (agent-shell-codex-history-older))
       (with-current-buffer source
         (map-put! (map-elt agent-shell--state :session) :id "s2"))
       (funcall callback (agent-shell-history-test--page "wrong session" nil))
       (should (= 1 (length agent-shell-codex-history--pages)))
       (should-not agent-shell-codex-history--loading)
       (should (string-match-p "source session changed" agent-shell-codex-history--error))))))

(ert-deftest agent-shell-history-rejects-cursor-loop-test ()
  "Repeated cursors do not append duplicate pages or trap the reader loading."
  (agent-shell-history-test--reader
   (lambda (_source)
     (cl-letf (((symbol-function 'acp-send-request)
                (lambda (&rest args)
                  (funcall (plist-get args :on-success)
                           (agent-shell-history-test--page "duplicate" "older")))))
       (agent-shell-codex-history-older))
     (should-not agent-shell-codex-history--loading)
     (should (= 1 (length agent-shell-codex-history--pages)))
     (should (string-match-p "repeated" agent-shell-codex-history--error)))))

(ert-deftest agent-shell-history-ignores-superseded-response-test ()
  "A reopened reader ignores callbacks for its previous request."
  (agent-shell-history-test--reader
   (lambda (_source)
     (let (callback)
       (cl-letf (((symbol-function 'acp-send-request)
                  (lambda (&rest args) (setq callback (plist-get args :on-success)))))
         (agent-shell-codex-history-older))
       (setq agent-shell-codex-history--request-token (list t))
       (funcall callback (agent-shell-history-test--page "stale" nil))
       (should (= 1 (length agent-shell-codex-history--pages)))))))

(ert-deftest agent-shell-history-tool-raw-output-test ()
  "Saved commands retain complete input and output without content blocks."
  (let* ((ticks (make-string 5 ?`))
         (input (concat "printf sample\n# " ticks))
         (output (concat "first line\n" ticks "\nlast line")))
    (dolist (raw-input (list input (list (cons 'command input))))
      (dolist (raw-output (list output (list (cons 'formatted_output output)
                                           '(exit_code . 7))))
        (with-temp-buffer
          (agent-shell-codex-history-mode)
          (let* ((update (list '(sessionUpdate . "tool_call_update")
                               '(toolCallId . "command-1")
                               (cons 'rawInput raw-input)
                               (cons 'rawOutput raw-output)))
                 (print-length 1)
                 (print-level 1))
            (setq agent-shell-codex-history--pages
                  (list (list '(version . 1) (cons 'updates (vector update)))))
            (agent-shell-codex-history--render-page)
            (dolist (text (list "Input" input "Output" output))
              (should (string-search text (buffer-string))))
            (unless (stringp raw-output)
              (should (string-search "exit_code" (buffer-string)))
              (should (string-search "7" (buffer-string))))))))))

(provide 'agent-shell-codex-history-tests)
;;; agent-shell-codex-history-tests.el ends here
