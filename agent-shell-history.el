;;; agent-shell-history.el --- Bounded agent history -*- lexical-binding: t; -*-

;;; Commentary:
;; Restore recent activity and read older pages without changing the live
;; conversation.  An unmodified adapter retains its usual full replay.

;;; Code:

(require 'agent-shell)
(require 'button)
(require 'pp)

(defvar agent-shell-codex-history-page-size nil
  "Number of recent Codex items to restore, or nil for normal ACP replay.
The local adapter accepts 1 through 100 items per page.")

(defconst agent-shell-history--meta-key 'agent-shell.dev/history)
(defvar agent-shell-openclaw-history-page-size nil
  "Number of recent OpenClaw events to restore, or nil for full replay.
The local adapter accepts 1 through 100 events per page.")

(defun agent-shell-history--page-size (state)
  "Return the requested history page size for STATE's agent."
  (pcase (map-nested-elt state '(:agent-config :identifier))
    ('codex agent-shell-codex-history-page-size)
    ('openclaw agent-shell-openclaw-history-page-size)))

(defvar-local agent-shell-history--page nil)
(defvar-local agent-shell-history--source nil)
(defvar-local agent-shell-history--session-id nil)
(defvar-local agent-shell-history--pages nil)
(defvar-local agent-shell-history--index 0)
(defvar-local agent-shell-history--loading nil)
(defvar-local agent-shell-history--error nil)
(defvar-local agent-shell-history--request-token nil)
(defvar-local agent-shell-history--rendered-pages nil
  "Rendered text keyed by page identity in this reader.")

(defun agent-shell-history--notification (session-id update)
  "Wrap UPDATE as an ACP history notification for SESSION-ID."
  (list '(method . "session/update")
        (cons 'params (list (cons 'sessionId session-id)
                            (cons 'update update)))))

(defun agent-shell-history--send-request-a (original &rest args)
  "Request bounded agent history through ORIGINAL with ARGS."
  (let ((state (plist-get args :state))
        (request (plist-get args :request)))
    (when (equal (map-elt request :method) "session/load")
      (setq-local agent-shell-history--page nil))
    (if (not (and (agent-shell-history--page-size state)
                  (equal (map-elt request :method) "session/load")))
        (apply original args)
      (let* ((request (copy-tree request))
             (params (map-elt request :params))
             (meta (map-elt params '_meta))
             (on-success (plist-get args :on-success))
             (session-id (map-elt params 'sessionId)))
        (setf (map-elt meta agent-shell-history--meta-key)
              (list '(version . 1) (cons 'limit (agent-shell-history--page-size state))))
        (setf (map-elt params '_meta) meta)
        (map-put! request :params params)
        (setq args (plist-put args :request request))
        (setq args
              (plist-put
               args :on-success
               (lambda (response)
                 (let ((page (map-nested-elt response
                                            (list '_meta agent-shell-history--meta-key))))
                   (when (equal (map-elt page 'version) 1)
                     (setq-local agent-shell-history--page page)
                     (let ((current (agent-shell--pop-pending-restore state)))
                       (map-put! state :pending-restore (agent-shell--make-pending-restore))
                       (seq-doseq (update (map-elt page 'updates))
                         (agent-shell--append-restore-notification
                          state (agent-shell-history--notification session-id update)))
                       (seq-doseq (turn current)
                         (seq-doseq (notification turn)
                           (agent-shell--append-restore-notification state notification))))
                     (agent-shell-history--insert-link state))
                   (funcall on-success response)))))
        (apply original args)))))

(defun agent-shell-history--insert-link (state)
  "Add access to STATE's older history above the recent conversation."
  (when (map-elt agent-shell-history--page 'nextCursor)
    (agent-shell--update-bootstrapping-fragment
     :state state :block-id "agent_older_history" :expanded t
     :body (concat "Showing recent activity. "
                   (agent-shell--make-button
                    :text "Browse older history"
                    :help "Read earlier messages without interrupting this session"
                    :action #'agent-shell-history-open)))))

(defvar agent-shell-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "n") #'agent-shell-history-older)
    (define-key map (kbd "p") #'agent-shell-history-newer)
    map))

(define-derived-mode agent-shell-history-mode special-mode "Agent History"
  "Read saved agent history; n loads older items, p returns to newer ones."
  (setq-local truncate-lines nil))

(defun agent-shell-history-open ()
  "Open a paginated reader for this agent session's saved history."
  (interactive)
  (unless agent-shell-history--page
    (user-error "This session was restored with complete history"))
  (let ((source (current-buffer))
        (page agent-shell-history--page)
        (session-id (map-nested-elt (agent-shell--state) '(:session :id))))
    (pop-to-buffer (get-buffer-create (format "*Agent history: %s*" session-id)))
    (unless (and (eq agent-shell-history--source source)
                 (eq (car agent-shell-history--pages) page))
      (agent-shell-history-mode)
      (setq agent-shell-history--source source
            agent-shell-history--session-id session-id
            agent-shell-history--index 0
            agent-shell-history--pages (list page)))
    (agent-shell-history--render-page)))

(defun agent-shell-history--content (content)
  "Return readable Markdown for tool CONTENT, including file changes."
  (pcase (map-elt content 'type)
    ("content" (agent-shell--content-block-to-markdown (map-elt content 'content)))
    ("diff" (format "%s\n\nBefore:\n\n%s\n\nAfter:\n\n%s"
                    (map-elt content 'path)
                    (or (map-elt content 'oldText) "")
                    (or (map-elt content 'newText) "")))
    (_ (pp-to-string content))))

(defun agent-shell-history--update-text (update)
  "Return readable Markdown for the saved ACP UPDATE."
  (let ((kind (map-elt update 'sessionUpdate)))
    (cond
     ((member kind '("user_message_chunk" "agent_message_chunk" "agent_thought_chunk"))
      (concat (pcase kind
                ("user_message_chunk" "User")
                ("agent_message_chunk" "Agent")
                (_ "Thought"))
              "\n\n" (agent-shell--content-block-to-markdown (map-elt update 'content))))
     ((member kind '("tool_call" "tool_call_update"))
      (concat (or (map-elt update 'title) (map-elt update 'toolCallId) "Tool")
              "\n\n"
              (mapconcat #'agent-shell-history--content
                         (map-elt update 'content) "\n\n")
              (mapconcat
               (lambda (entry)
                 (when (cdr entry)
                   (concat "\n\n" (car entry) "\n\n"
                           (agent-shell-history--literal (cdr entry)))))
               (list (cons "Input" (map-elt update 'rawInput))
                     (cons "Output" (map-elt update 'rawOutput)))
               "")))
     (t (pp-to-string update)))))

(defun agent-shell-history--literal (value)
  "Format saved tool VALUE as literal text in a Markdown fence.
Choose a fence longer than any backtick run in VALUE."
  (let ((text (if (stringp value) value
                (let ((print-length nil) (print-level nil)
                      (pp-escape-newlines nil))
                  (pp-to-string value))))
        (length 3)
        (start 0))
    (while (string-match "`+" text start)
      (setq length (max length (1+ (- (match-end 0) (match-beginning 0))))
            start (match-end 0)))
    (let ((fence (make-string length ?`)))
      (concat fence "\n" text "\n" fence))))

(defun agent-shell-history--render-page ()
  "Display the selected history page and navigation controls."
  (let ((inhibit-read-only t)
        (page (nth agent-shell-history--index agent-shell-history--pages)))
    (erase-buffer)
    (insert (format "Saved history · Page %d\n\n" (1+ agent-shell-history--index)))
    (when (> agent-shell-history--index 0)
      (insert-text-button "Newer (p)" 'action (lambda (_) (agent-shell-history-newer)))
      (insert "   "))
    (when (map-elt page 'nextCursor)
      (insert-text-button "Older (n)" 'action (lambda (_) (agent-shell-history-older)))
      (insert "   "))
    (insert-text-button "Back to conversation" 'action
                        (lambda (_)
                          (if (buffer-live-p agent-shell-history--source)
                              (pop-to-buffer agent-shell-history--source)
                            (user-error "The conversation buffer is closed"))))
    (insert "\n\n")
    (when agent-shell-history--error
      (insert "Could not load older history: " agent-shell-history--error
              "\nPress n to retry.\n\n"))
    (unless (assq page agent-shell-history--rendered-pages)
      (push (cons page
                  (mapconcat (lambda (update)
                               (concat (agent-shell-markdown-convert
                                        (agent-shell-history--update-text update))
                                       "\n\n"))
                             (map-elt page 'updates) ""))
            agent-shell-history--rendered-pages))
    (insert (cdr (assq page agent-shell-history--rendered-pages)))
    (when (zerop (length (map-elt page 'updates)))
      (insert "No visible messages on this page.\n"))
    (setq header-line-format (if agent-shell-history--loading "Loading older history…" nil))
    (goto-char (point-min))))

(defun agent-shell-history-newer ()
  "Display a more recent history page."
  (interactive)
  (unless agent-shell-history--loading
    (setq agent-shell-history--index (max 0 (1- agent-shell-history--index)))
    (agent-shell-history--render-page)))

(defun agent-shell-history-older ()
  "Read one older page, leaving the live session and its prompt unchanged."
  (interactive)
  (unless agent-shell-history--loading
    (if (< (1+ agent-shell-history--index) (length agent-shell-history--pages))
        (progn
          (setq agent-shell-history--index (1+ agent-shell-history--index))
          (agent-shell-history--render-page))
      (agent-shell-history--fetch-older))))

(defun agent-shell-history--fetch-older ()
  "Asynchronously fetch the next older page from the live session's client."
  (unless (buffer-live-p agent-shell-history--source)
    (user-error "Reopen the conversation to load more history"))
  (let* ((source agent-shell-history--source)
         (state (buffer-local-value 'agent-shell--state source))
         (source-page (buffer-local-value 'agent-shell-history--page source))
         (session-id agent-shell-history--session-id)
         (cursor (map-elt (car (last agent-shell-history--pages)) 'nextCursor))
         (buffer (current-buffer))
         (token (list t))
         (on-failure
          (lambda (err &rest _)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (when (eq token agent-shell-history--request-token)
                  (setq agent-shell-history--loading nil
                        agent-shell-history--request-token nil
                        agent-shell-history--error
                        (or (map-elt err 'message) (format "%s" err)))
                  (agent-shell-history--render-page)))))))
    (unless (equal session-id (map-nested-elt state '(:session :id)))
      (user-error "The source buffer has switched to another session"))
    (unless cursor (user-error "Beginning of history"))
    (setq agent-shell-history--loading t
          agent-shell-history--request-token token
          agent-shell-history--error nil
          header-line-format "Loading older history…")
    (condition-case err
        (acp-send-request
         :client (map-elt state :client) :buffer buffer
         :request (list (cons :method "_agent-shell/history")
                        (cons :params (list (cons 'sessionId session-id)
                                            (cons 'cursor cursor)
                                            (cons 'limit (or (agent-shell-history--page-size state) 40)))))
         :on-success
         (lambda (page)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (eq token agent-shell-history--request-token)
                 (cond
                  ((not (and (buffer-live-p source)
                             (eq state (buffer-local-value 'agent-shell--state source))
                             (eq source-page (buffer-local-value 'agent-shell-history--page source))
                             (equal session-id (map-nested-elt state '(:session :id)))))
                   (funcall on-failure '((message . "The source session changed; reopen history"))))
                  ((not (and (equal (map-elt page 'version) 1)
                             (let ((updates (map-elt page 'updates)))
                               (and (or (listp updates) (vectorp updates))
                                    (seq-every-p
                                     (lambda (update)
                                       (and (listp update)
                                            (stringp (map-elt update 'sessionUpdate))))
                                     updates)))
                             (or (null (map-elt page 'nextCursor))
                                 (stringp (map-elt page 'nextCursor)))))
                   (funcall on-failure '((message . "Invalid history page"))))
                  ((and (map-elt page 'nextCursor)
                        (seq-some (lambda (old)
                                    (equal (map-elt old 'nextCursor) (map-elt page 'nextCursor)))
                                  agent-shell-history--pages))
                   (funcall on-failure '((message . "Agent repeated a history cursor"))))
                  (t
                   (setq agent-shell-history--loading nil
                         agent-shell-history--request-token nil
                         agent-shell-history--pages
                         (append agent-shell-history--pages (list page))
                         agent-shell-history--index (1+ agent-shell-history--index))
                   (agent-shell-history--render-page)))))))
         :on-failure on-failure)
      (error (funcall on-failure (list (cons 'message (error-message-string err))))))))

(unless (advice-member-p #'agent-shell-history--send-request-a
                         'agent-shell--send-request)
  (advice-add 'agent-shell--send-request :around
              #'agent-shell-history--send-request-a))

(provide 'agent-shell-history)
;;; agent-shell-history.el ends here
