;;; agent-shell-session-push-tests.el --- Tests for session/push -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

;;; Code:

(ert-deftest agent-shell-session-push-status-lifecycle-test ()
  "An accepted push is acknowledged once and busy until session_push_end."
  (with-temp-buffer
    (let* ((agent-shell-show-busy-indicator t)
           (responses nil)
           (heartbeat-started nil)
           (heartbeat-stopped nil)
           (finished nil)
           (state (list (cons :buffer (current-buffer))
                        (cons :client 'test-client)
                        (cons :heartbeat 'test-heartbeat)
                        (cons :tool-calls nil)
                        (cons :active-requests nil)
                        (cons :last-entry-type nil))))
      (setq-local agent-shell--state state)
      (cl-letf (((symbol-function 'agent-shell--state) (lambda () state))
                ((symbol-function 'shell-maker-busy) (lambda () nil))
                ((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) responses)))
                ((symbol-function 'agent-shell-heartbeat-start)
                 (lambda (&rest _args)
                   (setq heartbeat-started t)))
                ((symbol-function 'agent-shell-heartbeat-stop)
                 (lambda (&rest _args)
                   (setq heartbeat-stopped t))))
        (should (eq (agent-shell-status) 'ready))
        (agent-shell-experimental--on-session-push-request
         :state state
         :acp-request '((id . 17) (method . "session/push")))
        (should heartbeat-started)
        (should (= (length responses) 1))
        (should (equal (map-elt (car responses) :request-id) 17))
        (should (assq :result (car responses)))
        (should-not (map-elt (car responses) :error))
        (should (eq (agent-shell-status) 'busy))
        (should (agent-shell-experimental--session-push-active-p state))
        (should (agent-shell--render-streaming-chunk-p
                 state "agent_message_chunk"))
        (agent-shell-experimental--on-session-push-end
         :state state
         :on-finished (lambda () (setq finished t)))
        (should heartbeat-stopped)
        (should finished)
        (should (= (length responses) 1))
        (should (eq (agent-shell-status) 'ready))
        (should-not (agent-shell-experimental--session-push-active-p state))
        (should (equal agent-shell--version
                       "0.54.1-openclaw-push-ack.1"))))))

(ert-deftest agent-shell-session-push-rejects-overlap-test ()
  "A push received during an active prompt or push is rejected as busy."
  (dolist (active-method '("session/prompt" "session/push"))
    (let* ((response nil)
           (active-request `((:id . 3) (:method . ,active-method)))
           (state (list (cons :client 'test-client)
                        (cons :active-requests (list active-request))
                        (cons :last-entry-type nil))))
      (cl-letf (((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (setq response (plist-get args :response)))))
        (agent-shell-experimental--on-session-push-request
         :state state
         :acp-request '((id . 18) (method . "session/push")))
        (should (equal (map-elt response :request-id) 18))
        (should (equal (map-nested-elt response '(:error message)) "Busy"))
        (should (equal (map-elt state :active-requests)
                       (list active-request)))))))

(ert-deftest agent-shell-session-push-process-exit-clears-busy-test ()
  "An ACP process exit clears a push that never receives its end event."
  (with-temp-buffer
    (let* ((agent-shell-show-busy-indicator nil)
           (process (make-process
                     :name (generate-new-buffer-name "agent-shell-push-test")
                     :command '("cat")
                     :noquery t))
           (state (list (cons :buffer (current-buffer))
                        (cons :client (list (cons :process process)))
                        (cons :heartbeat nil)
                        (cons :tool-calls nil)
                        (cons :active-requests nil)
                        (cons :last-entry-type nil))))
      (setq-local agent-shell--state state)
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell--state) (lambda () state))
                    ((symbol-function 'shell-maker-busy) (lambda () nil))
                    ((symbol-function 'acp-send-response)
                     (lambda (&rest _args) nil)))
            (agent-shell-experimental--on-session-push-request
             :state state
             :acp-request '((id . 19) (method . "session/push")))
            (should (eq (agent-shell-status) 'busy))
            (delete-process process)
            (accept-process-output process 0.1)
            (should (eq (agent-shell-status) 'ready))
            (should-not
             (agent-shell-experimental--session-push-active-p state)))
        (when (process-live-p process)
          (delete-process process))))))

(ert-deftest agent-shell-session-push-viewport-is-busy-test ()
  "Viewport busy state follows push-aware shell status."
  (with-temp-buffer
    (let ((shell-buffer (current-buffer)))
      (cl-letf (((symbol-function 'agent-shell--shell-buffer)
                 (lambda (&rest _args) shell-buffer))
                ((symbol-function 'agent-shell-status)
                 (lambda (&rest _args) 'busy)))
        (should (agent-shell-viewport--busy-p))))))

(provide 'agent-shell-session-push-tests)
;;; agent-shell-session-push-tests.el ends here
