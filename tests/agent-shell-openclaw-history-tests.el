;;; agent-shell-openclaw-history-tests.el --- History tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-openclaw)
(require 'agent-shell-codex-history)

(ert-deftest agent-shell-history-migrates-live-codex-state-test ()
  "An existing Codex reader can update without losing its page or draft."
  (with-temp-buffer
    (insert "Codex> unsent draft")
    (let ((old (intern "agent-shell-codex-history--page"))
          (page '((version . 1) (updates . []))))
      (make-variable-buffer-local old)
      (set (make-local-variable old) page)
      (agent-shell-codex-history--migrate-buffers)
      (should (eq agent-shell-history--page page))
      (should (equal (buffer-string) "Codex> unsent draft"))
      (setq-local agent-shell-history--page 'new)
      (agent-shell-codex-history--migrate-buffers)
      (should (eq agent-shell-history--page 'new))
      (should (eq (indirect-function 'agent-shell-codex-history-open)
                  (indirect-function 'agent-shell-history-open))))))

(ert-deftest agent-shell-history-caches-rendered-pages-test ()
  "Returning to a saved page does not repeat Markdown conversion."
  (with-temp-buffer
    (agent-shell-history-mode)
    (setq agent-shell-history--pages
          '(((updates . [((sessionUpdate . "agent_message_chunk")
                          (content . ((type . "text") (text . "recent"))))]))
            ((updates . [((sessionUpdate . "agent_message_chunk")
                          (content . ((type . "text") (text . "old"))))]))))
    (let ((calls 0))
      (cl-letf (((symbol-function 'agent-shell-markdown-convert)
                 (lambda (text) (setq calls (1+ calls)) text)))
        (agent-shell-history--render-page)
        (agent-shell-history-older)
        (agent-shell-history-newer)
        (should (string-search "recent" (buffer-string)))
        (agent-shell-history-older)
        (should (string-search "old" (buffer-string)))
        (should (= calls 2))))))

(ert-deftest agent-shell-history-clears-stale-page-on-full-load-test ()
  "Disabling pagination cannot leave the previous conversation's page."
  (with-temp-buffer
    (setq-local agent-shell-history--page 'old)
    (let ((agent-shell-openclaw-history-page-size nil))
      (agent-shell-history--send-request-a
       #'ignore :state (agent-shell--make-state :agent-config '((:identifier . openclaw)))
       :request '((:method . "session/load"))))
    (should-not agent-shell-history--page)))

(ert-deftest agent-shell-history-agent-settings-test ()
  "Each supported agent selects its own page size."
  (let ((agent-shell-codex-history-page-size 12)
        (agent-shell-openclaw-history-page-size 40))
    (dolist (entry '((codex . 12) (openclaw . 40) (other)))
      (should (equal (cdr entry)
                     (agent-shell-history--page-size
                      (list (list :agent-config (cons :identifier (car entry))))))))))

(ert-deftest agent-shell-history-openclaw-orders-current-controls-last-test ()
  "Saved controls must precede the current snapshot during replay."
  (with-temp-buffer
    (let* ((state (agent-shell--make-state :buffer (current-buffer)
                                          :agent-config '((:identifier . openclaw))))
           (agent-shell-openclaw-history-page-size 40)
           (request (acp-make-session-load-request :session-id "s1" :cwd "/tmp"))
           (before (copy-tree request))
           (old '((sessionUpdate . "current_mode_update") (currentModeId . "old")))
           (fresh '((sessionUpdate . "current_mode_update") (currentModeId . "xhigh"))))
      (map-put! state :pending-restore (agent-shell--make-pending-restore))
      (agent-shell--append-restore-notification
       state (agent-shell-history--notification "s1" fresh))
      (agent-shell-history--send-request-a
       (lambda (&rest args)
         (should (= 40 (map-nested-elt (plist-get args :request)
                                      '(:params _meta agent-shell.dev/history limit))))
         (funcall (plist-get args :on-success)
                  (list (cons '_meta
                              (list (cons 'agent-shell.dev/history
                                          (list '(version . 1) (cons 'updates (vector old)))))))))
       :state state :request request :on-success #'ignore)
      (should (equal before request))
      (should (equal (mapcar (lambda (notification)
                              (map-nested-elt notification '(params update currentModeId)))
                            (car (agent-shell--pop-pending-restore state)))
                     '("old" "xhigh"))))))

(ert-deftest agent-shell-openclaw-load-error-preserves-selection-test ()
  "A failed load displays an error without invoking the new-session fallback."
  (with-temp-buffer
    (insert "OpenClaw> unsent draft")
    (let ((state (agent-shell--make-state :buffer (current-buffer)
                                         :agent-config '((:identifier . openclaw))))
          (displayed nil)
          (fallback nil))
      (map-put! (map-elt state :session) :id "chosen")
      (map-put! state :pending-restore (agent-shell--make-pending-restore))
      (cl-letf (((symbol-function 'agent-shell--make-error-handler)
                 (lambda (&rest _) (lambda (&rest _) (setq displayed t)))))
        (agent-shell-openclaw--load-error-a
         (lambda (&rest args)
           (funcall (plist-get args :on-failure) '((message . "offline")) "offline"))
         :state state :request '((:method . "session/load"))
         :on-failure (lambda (&rest _) (setq fallback t))))
      (should displayed)
      (should-not fallback)
      (should-not (map-elt state :pending-restore))
      (should (equal "chosen" (map-nested-elt state '(:session :id))))
      (should (equal "OpenClaw> unsent draft" (buffer-string))))))

(ert-deftest agent-shell-openclaw-other-errors-retain-handler-test ()
  "The error override is limited to OpenClaw load and resume."
  (dolist (entry '((openclaw . "session/prompt") (codex . "session/load")))
    (let ((handler #'ignore))
      (agent-shell-openclaw--load-error-a
       (lambda (&rest args) (should (eq handler (plist-get args :on-failure))))
       :state (agent-shell--make-state
               :agent-config (list (cons :identifier (car entry))))
       :request (list (cons :method (cdr entry))) :on-failure handler))))

(provide 'agent-shell-openclaw-history-tests)
;;; agent-shell-openclaw-history-tests.el ends here
