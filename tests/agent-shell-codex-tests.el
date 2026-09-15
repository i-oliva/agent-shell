;;; agent-shell-codex-tests.el --- Tests for Codex session helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-codex)

(ert-deftest agent-shell-codex-profile-environment-pins-provider-test ()
  "The Codex ACP environment pins the provider for start and resume."
  (should
   (equal
    (agent-shell-codex-make-profile-environment
     :codex-home "/tmp/codex"
     :codex-path "/usr/bin/codex"
     :model-provider "opencodex")
    '("CODEX_HOME=/tmp/codex"
      "CODEX_PATH=/usr/bin/codex"
      "MODEL_PROVIDER=opencodex"
      "CODEX_API_KEY="
      "OPENAI_BASE_URL="
      "OPENAI_ORG_ID="
      "OPENAI_PROJECT_ID="))))

(ert-deftest agent-shell-codex-start-agent-avoids-current-line-context-test ()
  "The Codex launcher does not insert the source buffer's current line."
  (let ((agent-shell-context-sources '(files region error line))
        captured-sources)
    (cl-letf (((symbol-function 'agent-shell-openai-make-codex-config)
               (lambda () 'codex-config))
              ((symbol-function 'agent-shell--dwim)
               (lambda (&rest _args)
                 (setq captured-sources agent-shell-context-sources)
                 'started)))
      (should (eq (agent-shell-codex-start-agent) 'started)))
    (should (equal captured-sources '(files region error)))))

(ert-deftest agent-shell-codex-picker-switches-to-live-session-test ()
  "Selecting a thread already open in Emacs switches to its buffer."
  (let ((bootstrapping (generate-new-buffer " *codex-bootstrap*"))
        (live (generate-new-buffer " *codex-live*"))
        displayed)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq-local agent-shell--state
                        '((:agent-config . ((:identifier . codex)))
                          (:session . ((:id . "session-1"))))))
          (with-current-buffer bootstrapping
            (setq-local agent-shell--state
                        '((:agent-config . ((:identifier . codex)))))
            (cl-letf (((symbol-function 'agent-shell-buffers)
                       (lambda () (list live)))
                      ((symbol-function 'agent-shell--display-buffer)
                       (lambda (buffer) (setq displayed buffer))))
              (should
               (eq (agent-shell-codex--select-live-session-a
                    (lambda (_sessions) '((sessionId . "session-1")))
                    nil)
                   :other-shell))))
          (should (eq displayed live))
          (should-not (buffer-live-p bootstrapping)))
      (when (buffer-live-p bootstrapping) (kill-buffer bootstrapping))
      (when (buffer-live-p live) (kill-buffer live)))))

(ert-deftest agent-shell-codex-picker-returns-closed-session-test ()
  "A closed Codex thread remains available to the normal ACP load path."
  (let ((agent-shell--state '((:agent-config . ((:identifier . codex))))))
    (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil)))
      (let ((session '((sessionId . "session-2"))))
        (should (eq (agent-shell-codex--select-live-session-a
                     (lambda (_sessions) session) nil)
                    session))))))

(ert-deftest agent-shell-codex-picker-does-not-affect-other-agents-test ()
  "Codex live-buffer reuse does not intercept another agent's picker."
  (let ((agent-shell--state '((:agent-config . ((:identifier . openclaw)))))
        (session '((sessionId . "shared-id"))))
    (cl-letf (((symbol-function 'agent-shell-codex--buffer-for-session)
               (lambda (_session-id) (current-buffer))))
      (should (eq (agent-shell-codex--select-live-session-a
                   (lambda (_sessions) session) nil)
                  session)))))

(ert-deftest agent-shell-codex-load-failure-does-not-start-new-thread-test ()
  "A selected Codex thread load failure does not create a replacement."
  (with-temp-buffer
    (let* ((state (list (cons :agent-config '((:identifier . codex)))
                        (cons :buffer (current-buffer))
                        (cons :pending-restore t)))
           wrapped-failure
           finished
           original-failure-called)
      (cl-letf (((symbol-function 'agent-shell--update-bootstrapping-fragment)
                 #'ignore)
                ((symbol-function 'agent-shell-heartbeat-stop) #'ignore)
                ((symbol-function 'shell-maker-finish-output)
                 (lambda (&rest args)
                   (setq finished (plist-get args :success)))))
        (agent-shell-codex--send-request-a
         (lambda (&rest args)
           (setq wrapped-failure (plist-get args :on-failure)))
         :state state
         :request '((:method . "session/load"))
         :on-failure (lambda (&rest _)
                       (setq original-failure-called t)))
        (funcall wrapped-failure
                 '((message . "Internal error")
                   (data . ((details . "thread already has an active writer"))))
                 nil))
      (should-not original-failure-called)
      (should (eq finished nil))
      (should-not (map-elt state :pending-restore)))))

(provide 'agent-shell-codex-tests)
;;; agent-shell-codex-tests.el ends here
