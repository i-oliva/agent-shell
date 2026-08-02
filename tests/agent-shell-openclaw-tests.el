;;; agent-shell-openclaw-tests.el --- Tests for agent-shell-openclaw -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-openclaw)

;;; Code:

(ert-deftest agent-shell-openclaw-config-shape-test ()
  "Test OpenClaw config shape."
  (let ((config (agent-shell-openclaw-make-agent-config)))
    (should (eq (map-elt config :identifier) 'openclaw))
    (should (string= (map-elt config :mode-line-name) "OpenClaw"))
    (should (string= (map-elt config :buffer-name) "OpenClaw"))
    (should (string= (map-elt config :shell-prompt) "OpenClaw> "))
    (should (string= (map-elt config :shell-prompt-regexp) "OpenClaw> "))
    (should (string-match-p "docs\\.openclaw\\.ai" (map-elt config :install-instructions)))
    (should (functionp (map-elt config :client-maker)))
    (should-not (map-elt config :default-model-id))
    (should-not (map-elt config :default-session-mode-id))))

(ert-deftest agent-shell-openclaw-no-default-model-or-session-mode-test ()
  "Test OpenClaw does not expose unsupported model or session mode defaults."
  (let ((config (agent-shell-openclaw-make-agent-config)))
    (should-not (functionp (map-elt config :default-model-id)))
    (should-not (functionp (map-elt config :default-session-mode-id)))))

(ert-deftest agent-shell-openclaw-make-client-delegates-test ()
  "Test OpenClaw client construction delegates to `agent-shell--make-acp-client'."
  (let* ((agent-shell-openclaw-acp-command '("openclaw" "acp" "--url" "http://gateway.example"))
         (agent-shell-openclaw-environment '("OPENCLAW_GATEWAY_TOKEN=test-token"))
         (test-buffer (get-buffer-create "*agent-shell-openclaw-test*"))
         captured-args)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'agent-shell--make-acp-client)
                     (lambda (&rest args)
                       (setq captured-args args)
                       'mock-openclaw-client)))
            (should (eq (agent-shell-openclaw-make-client :buffer test-buffer)
                        'mock-openclaw-client)))
          (should (equal (plist-get captured-args :command) "openclaw"))
          (should (equal (plist-get captured-args :command-params)
                         '("acp" "--url" "http://gateway.example")))
          (should (equal (plist-get captured-args :environment-variables)
                         '("OPENCLAW_GATEWAY_TOKEN=test-token")))
          (should (eq (plist-get captured-args :context-buffer) test-buffer)))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

(ert-deftest agent-shell-openclaw-make-client-missing-buffer-test ()
  "Test OpenClaw client construction requires a buffer."
  (should-error (agent-shell-openclaw-make-client) :type 'error))

(ert-deftest agent-shell-openclaw-default-configs-include-openclaw-test ()
  "Test default agent configs include OpenClaw."
  (should (seq-some (lambda (config)
                      (eq (map-elt config :identifier) 'openclaw))
                    (agent-shell--resolved-agent-configs))))

(ert-deftest agent-shell-openclaw-start-agent-avoids-current-line-context-test ()
  "OpenClaw-specific launcher override avoids accidental current-line context."
  (let ((agent-shell-context-sources '(files region error line))
        captured-sources
        captured-config)
    (cl-letf (((symbol-function 'agent-shell--dwim)
               (lambda (&rest args)
                 (setq captured-sources agent-shell-context-sources)
                 (setq captured-config (plist-get args :config))
                 'started)))
      (should (eq (agent-shell-openclaw-start-agent) 'started)))
    (should (equal captured-sources '(files region error)))
    (should (eq (map-elt captured-config :identifier) 'openclaw))))

(ert-deftest agent-shell-openclaw-set-session-mode-uses-native-set-mode-test ()
  "Without a mode config option, v0.66.1 changes the session mode natively."
  (let* ((state (list (cons :client 'test-client)
                      (cons :session
                            (list (cons :id "session-1")
                                  (cons :modes
                                        (list (list (cons :id "high")
                                                    (cons :name "High"))))
                                  (cons :mode-id "high")))))
         sent-request)
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () state))
              ((symbol-function 'agent-shell--send-request)
               (lambda (&rest args)
                 (setq sent-request (plist-get args :request))))
              ((symbol-function 'agent-shell--update-header-and-mode-line)
               #'ignore))
      (agent-shell--config-option-set-mode-id :mode-id "ultra"))
    (should (equal (map-elt sent-request :method) "session/set_mode"))
    (should (equal (map-nested-elt sent-request '(:params modeId)) "ultra"))))

(provide 'agent-shell-openclaw-tests)
;;; agent-shell-openclaw-tests.el ends here
