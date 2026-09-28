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
(require 'sqlite nil t)

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

(defvar agent-shell-codex-session-directory nil
  "Local Codex home whose session index can be used by the picker.
Set this to the same directory as the ACP process's CODEX_HOME, for
example ~/.codex/.  Nil leaves session discovery to the ACP agent.")

(defun agent-shell-codex--local-sessions (cwd)
  "Return (:sessions . SESSIONS) from the local Codex index for CWD.
Return nil if the index is unavailable, so ACP discovery can take over.
An empty index returns (:sessions), distinct from an unavailable index."
  (when (and agent-shell-codex-session-directory
             (not (file-remote-p agent-shell-codex-session-directory))
             (not (file-remote-p cwd))
             (fboundp 'sqlite-available-p)
             (sqlite-available-p))
    (condition-case err
        (when-let* ((directory (expand-file-name agent-shell-codex-session-directory))
                    (database
                     (car (sort (directory-files directory t "^state_[0-9]+\\.sqlite$")
                                (lambda (a b)
                                  (> (string-to-number (substring (file-name-base a) 6))
                                     (string-to-number (substring (file-name-base b) 6)))))))
                    (db (sqlite-open database t)))
          (unwind-protect
              (let ((root (file-truename (file-name-as-directory cwd)))
                    (roots (make-hash-table :test 'equal))
                    (columns (mapcar #'cadr (sqlite-select db "PRAGMA table_info(threads)")))
                    (provider
                     (with-temp-buffer
                       (insert-file-contents (expand-file-name "config.toml" directory))
                       (goto-char (point-min))
                       (let ((end (or (and (re-search-forward "^[ \t]*\\[" nil t)
                                           (match-beginning 0))
                                      (point-max))))
                         (goto-char (point-min))
                         (when (re-search-forward
                                "^[ \t]*model_provider[ \t]*=[ \t]*[\"']\\([^\"'\n]+\\)[\"']"
                                end t)
                           (match-string-no-properties 1))))))
                (cons
                 :sessions
                 (delq nil
                       (mapcar
                        (lambda (row)
                          (let ((session-cwd (nth 1 row)))
                            (unless (file-remote-p session-cwd)
                              (when (and (equal root
                                                (or (gethash session-cwd roots)
                                                    (puthash session-cwd
                                                             (file-truename
                                                              (file-name-as-directory session-cwd))
                                                             roots)))
                                         (not (file-remote-p (nth 4 row)))
                                         (file-readable-p (nth 4 row)))
                                (list (cons 'sessionId (nth 0 row))
                                      (cons 'cwd session-cwd)
                                      (cons 'title (nth 2 row))
                                      (cons 'updatedAt
                                            (format-time-string "%FT%TZ" (nth 3 row) t)))))))
                        (sqlite-select
                         db
                         (format "SELECT id, cwd, %s, updated_at, rollout_path FROM threads
WHERE archived = 0
  AND source IN ('cli', 'vscode', 'exec', 'appServer', 'unknown')
  AND (? IS NULL OR model_provider = ?)
ORDER BY updated_at DESC, id DESC"
                                 (if (and (member "name" columns) (member "preview" columns))
                                     "COALESCE(NULLIF(name, ''), NULLIF(preview, ''), title)"
                                   "title"))
                         (list provider provider))))))
            (sqlite-close db)))
      (error
       (message "Codex local session index unavailable (%s); using ACP"
                (error-message-string err))
       nil))))

(defun agent-shell-codex--list-sessions-a (original &rest args)
  "Use the local Codex session index, or call ORIGINAL with ARGS.
Other agents and remote directories always use ACP session discovery."
  (let ((local (and (eq (map-nested-elt (plist-get args :state)
                                       '(:agent-config :identifier))
                        'codex)
                    (agent-shell-codex--local-sessions (plist-get args :cwd)))))
    (if local
        (funcall (plist-get args :on-success) (cdr local))
      (apply original args))))

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

(unless (advice-member-p #'agent-shell-codex--list-sessions-a
                         'agent-shell--list-sessions)
  (advice-add 'agent-shell--list-sessions :around
              #'agent-shell-codex--list-sessions-a))

(provide 'agent-shell-codex)

;;; agent-shell-codex.el ends here
