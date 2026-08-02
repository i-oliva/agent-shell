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
    (should (string= (map-elt config :session-mode-config-category)
                     "thought_level"))
    (should-not (map-elt config :session-list-filter-by-cwd))
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

(ert-deftest agent-shell-openclaw-session-modes-use-thought-level-config-test ()
  "OpenClaw session modes follow the model-specific thought-level options."
  (let* ((config (agent-shell-openclaw-make-agent-config))
         (normalized-options
          (agent-shell--normalize-config-options
           [((id . "thought_level")
             (name . "Thought level")
             (category . "thought_level")
             (type . "select")
             (currentValue . "xhigh")
             (options . [((value . "off") (name . "Off"))
                         ((value . "high") (name . "High"))
                         ((value . "xhigh") (name . "Extra High"))
                         ((value . "max") (name . "Max"))
                         ((value . "ultra") (name . "Ultra"))]))]))
         (session (list (cons :id "session-1")
                        (cons :mode-id "high")
                        (cons :modes
                              (list (list (cons :id "off")
                                          (cons :name "Off"))
                                    (list (cons :id "high")
                                          (cons :name "High"))))
                        (cons :config-options normalized-options)))
         (state (list (cons :agent-config config)
                      (cons :session session)
                      (cons :config-options normalized-options))))
    (should (equal (mapcar (lambda (mode) (map-elt mode :id))
                            (agent-shell--get-available-modes state))
                   (list "off" "high" "xhigh" "max" "ultra")))
    (should (equal (agent-shell--current-mode-id state) "xhigh"))))

(ert-deftest agent-shell-openclaw-set-session-mode-uses-thought-level-option-test ()
  "OpenClaw mode changes use the thought_level config option."
  (let* ((config (agent-shell-openclaw-make-agent-config))
         (normalized-options
          (agent-shell--normalize-config-options
           [((id . "thought_level")
             (name . "Thought level")
             (category . "thought_level")
             (type . "select")
             (currentValue . "high")
             (options . [((value . "high") (name . "High"))
                         ((value . "ultra") (name . "Ultra"))]))]))
         (state (list (cons :agent-config config)
                      (cons :client (quote test-client))
                      (cons :session
                            (list (cons :id "session-1")
                                  (cons :config-options normalized-options)))
                      (cons :config-options normalized-options)))
         sent-request)
    (cl-letf (((symbol-function (quote agent-shell--state))
               (lambda () state))
              ((symbol-function (quote agent-shell--send-request))
               (lambda (&rest args)
                 (setq sent-request (plist-get args :request))))
              ((symbol-function (quote agent-shell--update-header-and-mode-line))
               (function ignore)))
      (agent-shell--config-option-set-mode-id :mode-id "ultra"))
    (should (equal (map-elt sent-request :method)
                   "session/set_config_option"))
    (should (equal (map-nested-elt sent-request (quote (:params configId)))
                   "thought_level"))
    (should (equal (map-nested-elt sent-request (quote (:params value)))
                   "ultra"))))

(provide 'agent-shell-openclaw-tests)
;;; agent-shell-openclaw-tests.el ends here
