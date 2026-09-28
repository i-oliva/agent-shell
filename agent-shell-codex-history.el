;;; agent-shell-codex-history.el --- Bounded Codex history -*- lexical-binding: t; -*-

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

(defconst agent-shell-codex-history--meta-key 'agent-shell.dev/history)
(defvar-local agent-shell-codex-history--page nil)
(defvar-local agent-shell-codex-history--source nil)
(defvar-local agent-shell-codex-history--session-id nil)
(defvar-local agent-shell-codex-history--pages nil)
(defvar-local agent-shell-codex-history--index 0)
(defvar-local agent-shell-codex-history--loading nil)
(defvar-local agent-shell-codex-history--error nil)
(defvar-local agent-shell-codex-history--request-token nil)

(defun agent-shell-codex-history--notification (session-id update)
  "Wrap UPDATE as an ACP history notification for SESSION-ID."
  (list '(method . "session/update")
        (cons 'params (list (cons 'sessionId session-id)
                            (cons 'update update)))))

(defun agent-shell-codex-history--send-request-a (original &rest args)
  "Request bounded Codex history through ORIGINAL with ARGS."
  (let ((state (plist-get args :state))
        (request (plist-get args :request)))
    (if (not (and agent-shell-codex-history-page-size
                  (eq (map-nested-elt state '(:agent-config :identifier)) 'codex)
                  (equal (map-elt request :method) "session/load")))
        (apply original args)
      (let* ((request (copy-tree request))
             (params (map-elt request :params))
             (meta (map-elt params '_meta))
             (on-success (plist-get args :on-success))
             (session-id (map-elt params 'sessionId)))
        (setf (map-elt meta agent-shell-codex-history--meta-key)
              (list '(version . 1) (cons 'limit agent-shell-codex-history-page-size)))
        (setf (map-elt params '_meta) meta)
        (map-put! request :params params)
        (setq args (plist-put args :request request))
        (setq args
              (plist-put
               args :on-success
               (lambda (response)
                 (let ((page (map-nested-elt response
                                            (list '_meta agent-shell-codex-history--meta-key))))
                   (when (equal (map-elt page 'version) 1)
                     (setq-local agent-shell-codex-history--page page)
                     (unless (map-elt state :pending-restore)
                       (map-put! state :pending-restore (agent-shell--make-pending-restore)))
                     (seq-doseq (update (map-elt page 'updates))
                       (agent-shell--append-restore-notification
                        state (agent-shell-codex-history--notification session-id update)))
                     (agent-shell-codex-history--insert-link state))
                   (funcall on-success response)))))
        (apply original args)))))

(defun agent-shell-codex-history--insert-link (state)
  "Add access to STATE's older history above the recent conversation."
  (when (map-elt agent-shell-codex-history--page 'nextCursor)
    (agent-shell--update-bootstrapping-fragment
     :state state :block-id "codex_older_history" :expanded t
     :body (concat "Showing recent activity. "
                   (agent-shell--make-button
                    :text "Browse older history"
                    :help "Read earlier messages without interrupting this session"
                    :action #'agent-shell-codex-history-open)))))

(defvar agent-shell-codex-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "n") #'agent-shell-codex-history-older)
    (define-key map (kbd "p") #'agent-shell-codex-history-newer)
    map))

(define-derived-mode agent-shell-codex-history-mode special-mode "Codex History"
  "Read saved Codex history; n loads older items, p returns to newer ones."
  (setq-local truncate-lines nil))

(defun agent-shell-codex-history-open ()
  "Open a paginated reader for this Codex session's saved history."
  (interactive)
  (unless agent-shell-codex-history--page
    (user-error "This session was restored with complete history"))
  (let ((source (current-buffer))
        (page agent-shell-codex-history--page)
        (session-id (map-nested-elt (agent-shell--state) '(:session :id))))
    (pop-to-buffer (get-buffer-create (format "*Codex history: %s*" session-id)))
    (unless (and (eq agent-shell-codex-history--source source)
                 (eq (car agent-shell-codex-history--pages) page))
      (agent-shell-codex-history-mode)
      (setq agent-shell-codex-history--source source
            agent-shell-codex-history--session-id session-id
            agent-shell-codex-history--index 0
            agent-shell-codex-history--pages (list page)))
    (agent-shell-codex-history--render-page)))

(defun agent-shell-codex-history--content (content)
  "Return readable Markdown for tool CONTENT, including file changes."
  (pcase (map-elt content 'type)
    ("content" (agent-shell--content-block-to-markdown (map-elt content 'content)))
    ("diff" (format "%s\n\nBefore:\n\n%s\n\nAfter:\n\n%s"
                    (map-elt content 'path)
                    (or (map-elt content 'oldText) "")
                    (or (map-elt content 'newText) "")))
    (_ (pp-to-string content))))

(defun agent-shell-codex-history--update-text (update)
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
              (mapconcat #'agent-shell-codex-history--content
                         (map-elt update 'content) "\n\n")
              (mapconcat
               (lambda (entry)
                 (when (cdr entry)
                   (concat "\n\n" (car entry) "\n\n"
                           (agent-shell-codex-history--literal (cdr entry)))))
               (list (cons "Input" (map-elt update 'rawInput))
                     (cons "Output" (map-elt update 'rawOutput)))
               "")))
     (t (pp-to-string update)))))

(defun agent-shell-codex-history--literal (value)
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

(defun agent-shell-codex-history--render-page ()
  "Display the selected history page and navigation controls."
  (let ((inhibit-read-only t)
        (page (nth agent-shell-codex-history--index agent-shell-codex-history--pages)))
    (erase-buffer)
    (insert (format "Saved history · Page %d\n\n" (1+ agent-shell-codex-history--index)))
    (when (> agent-shell-codex-history--index 0)
      (insert-text-button "Newer (p)" 'action (lambda (_) (agent-shell-codex-history-newer)))
      (insert "   "))
    (when (map-elt page 'nextCursor)
      (insert-text-button "Older (n)" 'action (lambda (_) (agent-shell-codex-history-older)))
      (insert "   "))
    (insert-text-button "Back to conversation" 'action
                        (lambda (_)
                          (if (buffer-live-p agent-shell-codex-history--source)
                              (pop-to-buffer agent-shell-codex-history--source)
                            (user-error "The conversation buffer is closed"))))
    (insert "\n\n")
    (when agent-shell-codex-history--error
      (insert "Could not load older history: " agent-shell-codex-history--error
              "\nPress n to retry.\n\n"))
    (seq-doseq (update (map-elt page 'updates))
      (insert (agent-shell-markdown-convert
               (agent-shell-codex-history--update-text update)) "\n\n"))
    (when (zerop (length (map-elt page 'updates)))
      (insert "No visible messages on this page.\n"))
    (setq header-line-format (if agent-shell-codex-history--loading "Loading older history…" nil))
    (goto-char (point-min))))

(defun agent-shell-codex-history-newer ()
  "Display a more recent history page."
  (interactive)
  (unless agent-shell-codex-history--loading
    (setq agent-shell-codex-history--index (max 0 (1- agent-shell-codex-history--index)))
    (agent-shell-codex-history--render-page)))

(defun agent-shell-codex-history-older ()
  "Read one older page, leaving the live session and its prompt unchanged."
  (interactive)
  (unless agent-shell-codex-history--loading
    (if (< (1+ agent-shell-codex-history--index) (length agent-shell-codex-history--pages))
        (progn
          (setq agent-shell-codex-history--index (1+ agent-shell-codex-history--index))
          (agent-shell-codex-history--render-page))
      (agent-shell-codex-history--fetch-older))))

(defun agent-shell-codex-history--fetch-older ()
  "Asynchronously fetch the next older page from the live session's client."
  (unless (buffer-live-p agent-shell-codex-history--source)
    (user-error "Reopen the conversation to load more history"))
  (let* ((source agent-shell-codex-history--source)
         (state (buffer-local-value 'agent-shell--state source))
         (session-id agent-shell-codex-history--session-id)
         (cursor (map-elt (car (last agent-shell-codex-history--pages)) 'nextCursor))
         (buffer (current-buffer))
         (token (list t))
         (on-failure
          (lambda (err &rest _)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (when (eq token agent-shell-codex-history--request-token)
                  (setq agent-shell-codex-history--loading nil
                        agent-shell-codex-history--request-token nil
                        agent-shell-codex-history--error
                        (or (map-elt err 'message) (format "%s" err)))
                  (agent-shell-codex-history--render-page)))))))
    (unless (equal session-id (map-nested-elt state '(:session :id)))
      (user-error "The source buffer has switched to another session"))
    (unless cursor (user-error "Beginning of history"))
    (setq agent-shell-codex-history--loading t
          agent-shell-codex-history--request-token token
          agent-shell-codex-history--error nil
          header-line-format "Loading older history…")
    (condition-case err
        (acp-send-request
         :client (map-elt state :client) :buffer buffer
         :request (list (cons :method "_agent-shell/history")
                        (cons :params (list (cons 'sessionId session-id)
                                            (cons 'cursor cursor)
                                            (cons 'limit (or agent-shell-codex-history-page-size 40)))))
         :on-success
         (lambda (page)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (eq token agent-shell-codex-history--request-token)
                 (cond
                  ((not (and (buffer-live-p source)
                             (eq state (buffer-local-value 'agent-shell--state source))
                             (equal session-id (map-nested-elt state '(:session :id)))))
                   (funcall on-failure '((message . "The source session changed; reopen history"))))
                  ((not (and (equal (map-elt page 'version) 1)
                             (sequencep (map-elt page 'updates))
                             (or (null (map-elt page 'nextCursor))
                                 (stringp (map-elt page 'nextCursor)))))
                   (funcall on-failure '((message . "Invalid history page"))))
                  ((and (map-elt page 'nextCursor)
                        (seq-some (lambda (old)
                                    (equal (map-elt old 'nextCursor) (map-elt page 'nextCursor)))
                                  agent-shell-codex-history--pages))
                   (funcall on-failure '((message . "Codex repeated a history cursor"))))
                  (t
                   (setq agent-shell-codex-history--loading nil
                         agent-shell-codex-history--request-token nil
                         agent-shell-codex-history--pages
                         (append agent-shell-codex-history--pages (list page))
                         agent-shell-codex-history--index (1+ agent-shell-codex-history--index))
                   (agent-shell-codex-history--render-page)))))))
         :on-failure on-failure)
      (error (funcall on-failure (list (cons 'message (error-message-string err))))))))

(unless (advice-member-p #'agent-shell-codex-history--send-request-a
                         'agent-shell--send-request)
  (advice-add 'agent-shell--send-request :around
              #'agent-shell-codex-history--send-request-a))

(provide 'agent-shell-codex-history)
;;; agent-shell-codex-history.el ends here
