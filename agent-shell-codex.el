;;; agent-shell-codex.el --- Safer Codex session handling -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Local Codex integration helpers.  Avoid inserting incidental current-line
;; context at startup, reuse sessions already open in this Emacs, and preserve
;; the real Codex load error instead of silently replacing a selected thread.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'agent-shell)
(require 'agent-shell-openai)

(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--dwim "agent-shell")
(declare-function agent-shell--prompt-select-session "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--update-bootstrapping-fragment "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-heartbeat-stop "agent-shell-heartbeat")
(declare-function shell-maker-finish-output "shell-maker")
(defvar agent-shell--state)
(defvar agent-shell-context-sources)
(defvar shell-maker--config)

(cl-defun agent-shell-codex-make-profile-environment
    (&key codex-home codex-path model-provider)
  "Return a clean Codex ACP environment for a local model provider.

CODEX-HOME selects the Codex profile, CODEX-PATH selects the Codex binary,
and MODEL-PROVIDER pins both new and resumed ACP sessions.  For example:

  (agent-shell-codex-make-profile-environment
   :codex-home \"/tmp/codex\"
   :codex-path \"/usr/bin/codex\"
   :model-provider \"local\")

returns entries beginning with \"CODEX_HOME=/tmp/codex\",
\"CODEX_PATH=/usr/bin/codex\", and \"MODEL_PROVIDER=local\"."
  (dolist (value (list codex-home codex-path model-provider))
    (unless (and (stringp value) (not (string-empty-p value)))
      (error "Codex profile environment values must be non-empty strings")))
  (list (format "CODEX_HOME=%s" codex-home)
        (format "CODEX_PATH=%s" codex-path)
        (format "MODEL_PROVIDER=%s" model-provider)
        "CODEX_API_KEY="
        "OPENAI_BASE_URL="
        "OPENAI_ORG_ID="
        "OPENAI_PROJECT_ID="))

(defun agent-shell-codex-start-agent ()
  "Start Codex without copying an incidental source line into its prompt."
  (interactive)
  (let ((agent-shell-context-sources '(files region error)))
    (agent-shell--dwim :config (agent-shell-openai-make-codex-config)
                       :new-shell t)))

(defun agent-shell-codex--buffer-for-session (session-id)
  "Return the live Codex Agent Shell buffer for SESSION-ID, if present."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (eq (map-nested-elt agent-shell--state
                                '(:agent-config :identifier))
                'codex)
            (equal (map-nested-elt agent-shell--state '(:session :id))
                   session-id))))
   (agent-shell-buffers)))

(defun agent-shell-codex--select-live-session-a (original sessions)
  "Switch to a live Codex session selected through ORIGINAL from SESSIONS."
  (let ((selection (funcall original sessions)))
    (if-let* (((eq (map-nested-elt agent-shell--state
                                   '(:agent-config :identifier))
                   'codex))
              ((listp selection))
              (session-id (map-elt selection 'sessionId))
              (live-buffer (agent-shell-codex--buffer-for-session session-id)))
        (let ((bootstrapping-buffer (current-buffer)))
          (agent-shell--display-buffer live-buffer)
          (when (and (buffer-live-p bootstrapping-buffer)
                     (not (eq bootstrapping-buffer live-buffer)))
            (kill-buffer bootstrapping-buffer))
          :other-shell)
      selection)))

(defun agent-shell-codex--request-method (request)
  "Return REQUEST's ACP method regardless of keyword or symbol key shape."
  (or (map-elt request :method) (map-elt request 'method)))

(defun agent-shell-codex--load-request-p (state request)
  "Return non-nil for a Codex session load or resume REQUEST using STATE."
  (and (eq (map-nested-elt state '(:agent-config :identifier)) 'codex)
       (member (agent-shell-codex--request-method request)
               '("session/load" "session/resume"))))

(defun agent-shell-codex--load-error-text (acp-error raw-message)
  "Return a useful Codex load error from ACP-ERROR and RAW-MESSAGE."
  (or (map-nested-elt acp-error '(data details))
      (map-elt acp-error 'message)
      (and (stringp raw-message) raw-message)
      "Codex could not open the selected session."))

(defun agent-shell-codex--send-request-a (original &rest args)
  "Keep Codex load failures visible instead of starting another thread.
ORIGINAL is the wrapped request function; ARGS are its keyword arguments."
  (let ((state (plist-get args :state))
        (request (plist-get args :request)))
    (if (not (agent-shell-codex--load-request-p state request))
        (apply original args)
      (let ((on-failure (plist-get args :on-failure)))
        (plist-put
         args :on-failure
         (lambda (acp-error raw-message)
           (map-put! state :pending-restore nil)
           (agent-shell-heartbeat-stop :heartbeat (map-elt state :heartbeat))
           (let ((detail (agent-shell-codex--load-error-text
                          acp-error raw-message)))
             (with-current-buffer (map-elt state :buffer)
               (agent-shell--update-bootstrapping-fragment
                :state state
                :block-id "restore_fallback"
                :body
                (agent-shell--make-boxed-message
                 :text
                 (concat
                  "Codex did not open this session: " detail
                  (when (string-match-p "active writer" detail)
                    "\n\nThe thread is open in another Codex or Emacs process. Close that session, then reopen it here.")))))
             (with-current-buffer (map-elt state :buffer)
               (shell-maker-finish-output :config shell-maker--config
                                          :success nil))
             (message "Codex session load failed: %s" detail))
           ;; Do not call the original callback.  Its fallback creates a new,
           ;; unrelated thread after a user explicitly selected an old one.
           (ignore on-failure)))
        (apply original args)))))

(unless (advice-member-p #'agent-shell-codex--select-live-session-a
                         'agent-shell--prompt-select-session)
  (advice-add 'agent-shell--prompt-select-session :around
              #'agent-shell-codex--select-live-session-a))

(unless (advice-member-p #'agent-shell-codex--send-request-a
                         'agent-shell--send-request)
  (advice-add 'agent-shell--send-request :around
              #'agent-shell-codex--send-request-a))

(provide 'agent-shell-codex)

;;; agent-shell-codex.el ends here
