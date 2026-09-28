;;; agent-shell-session-push-tests.el --- Tests for session/push -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

;;; Code:

(ert-deftest agent-shell-session-push-status-lifecycle-test ()
  "A push is tracked as active and answered on session_push_end (v0.74.3)."
  (with-temp-buffer
    (insert "Codex> ")
    (setq-local comint-last-prompt
                (cons (copy-marker (point-min)) (copy-marker (point-max))))
    (let* ((responses nil)
           (heartbeat-started nil)
           (heartbeat-stopped nil)
           (finished nil)
           (state (list (cons :buffer (current-buffer))
                        (cons :client 'test-client)
                        (cons :heartbeat 'test-heartbeat)
                        (cons :active-requests nil)
                        (cons :last-entry-type nil))))
      (cl-letf (((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) responses)))
                ((symbol-function 'agent-shell-heartbeat-start)
                 (lambda (&rest _args)
                   (setq heartbeat-started t)))
                ((symbol-function 'agent-shell-heartbeat-stop)
                 (lambda (&rest _args)
                   (setq heartbeat-stopped t)))
                ((symbol-function 'agent-shell-experimental--remove-trailing-prompt)
                 (lambda () nil))
                ((symbol-function 'shell-maker-insert-end-of-prompt-marker)
                 (lambda () nil)))
        (let ((agent-shell-show-busy-indicator t))
          (agent-shell-experimental--on-session-push-request
           :state state
           :acp-request '((id . 17) (method . "session/push"))))
        (should heartbeat-started)
        (should shell-maker--busy)
        (should (= (length (map-elt state :active-requests)) 1))
        (should (equal (map-elt state :last-entry-type) "session/push"))
        (should (= (length responses) 0))
        (agent-shell-experimental--on-session-push-end
         :state state
         :on-finished (lambda () (setq finished t)))
        (should heartbeat-stopped)
        (should finished)
        (should (= (length responses) 1))
        (should (equal (map-elt (car responses) :request-id) 17))
        (should (assq :result (car responses)))
        (should-not (map-elt (car responses) :error))
        (should-not (map-elt state :active-requests))
        (should (equal (map-elt state :last-entry-type) "session_push_end"))))))

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

(ert-deftest agent-shell-session-push-viewport-is-busy-test ()
  "Viewport busy state follows the shell busy variable (v0.74.3)."
  (with-temp-buffer
    (let ((shell-buffer (current-buffer)))
      (setq-local shell-maker--busy t)
      (cl-letf (((symbol-function 'agent-shell--shell-buffer)
                 (lambda (&rest _args) shell-buffer)))
        (should (agent-shell-viewport--busy-p))))))

(provide 'agent-shell-session-push-tests)
;;; agent-shell-session-push-tests.el ends here
