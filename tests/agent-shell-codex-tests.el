;;; agent-shell-codex-tests.el --- Tests for Codex session helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-codex)

(defun agent-shell-codex-test--with-index (test)
  "Call TEST with a temporary Codex home and writable session database."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (ert-skip "SQLite support unavailable"))
  (let* ((directory (make-temp-file "codex-index-" t))
         (agent-shell-codex-session-directory directory)
         (db (sqlite-open (expand-file-name "state_5.sqlite" directory) nil)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "config.toml" directory)
            (insert "model_provider = 'local'\n[profiles.other]\nmodel_provider = 'other'\n"))
          (with-temp-file (expand-file-name "rollout.jsonl" directory))
          (sqlite-execute
           db (format "CREATE TABLE threads (id TEXT, cwd TEXT, title TEXT,
updated_at INTEGER, archived INTEGER, source TEXT, model_provider TEXT,
rollout_path TEXT DEFAULT '%s')" (expand-file-name "rollout.jsonl" directory)))
          (funcall test directory db))
      (sqlite-close db)
      (delete-directory directory t))))

(ert-deftest agent-shell-codex-local-index-filters-and-sorts-test ()
  "Keep this project's current-provider root threads, including old ones."
  (agent-shell-codex-test--with-index
   (lambda (directory db)
     (dolist (row '(("old" 1 0 "cli" "local")
                    ("new" 9 0 "vscode" "local")
                    ("archived" 10 1 "cli" "local")
                    ("child" 11 0 "subagent" "local")
                    ("provider" 12 0 "cli" "other")))
       (sqlite-execute db "INSERT INTO threads (id,cwd,title,updated_at,archived,source,model_provider) VALUES (?, ?, ?, ?, ?, ?, ?)"
                       (append (list (car row) directory (car row)) (cdr row))))
     (sqlite-execute db "INSERT INTO threads (id,cwd,title,updated_at,archived,source,model_provider) VALUES ('elsewhere', '/', 'Other', 20, 0, 'cli', 'local')")
     (sqlite-execute db "INSERT INTO threads VALUES ('deleted', ?, 'Deleted', 30, 0, 'cli', 'local', '/missing/rollout.jsonl')"
                     (list directory))
     (let ((sessions (cdr (agent-shell-codex--local-sessions directory))))
       (should (equal (mapcar (lambda (s) (map-elt s 'sessionId)) sessions)
                      '("new" "old")))
       (should (equal (map-elt (car sessions) 'updatedAt) "1970-01-01T00:00:09Z"))))))

(ert-deftest agent-shell-codex-local-index-resolves-project-symlinks-test ()
  "Both project aliases and canonical paths find the same saved thread."
  (agent-shell-codex-test--with-index
   (lambda (directory db)
     (let ((alias (expand-file-name "alias" directory))
           (project (expand-file-name "project" directory)))
       (make-directory project)
       (make-symbolic-link project alias)
       (sqlite-execute db "INSERT INTO threads (id,cwd,title,updated_at,archived,source,model_provider) VALUES ('saved', ?, 'Saved', 1, 0, 'cli', 'local')"
                       (list alias))
       (should (= 1 (length (cdr (agent-shell-codex--local-sessions project)))))
       (should (= 1 (length (cdr (agent-shell-codex--local-sessions alias)))))))))

(ert-deftest agent-shell-codex-local-index-empty-skips-acp-test ()
  "A valid empty index returns promptly without a remote history scan."
  (agent-shell-codex-test--with-index
   (lambda (directory _db)
     (let ((result :not-called))
       (agent-shell-codex--list-sessions-a
        (lambda (&rest _) (ert-fail "Unexpected ACP scan"))
        :state '((:agent-config . ((:identifier . codex))))
        :cwd directory :on-success (lambda (sessions) (setq result sessions)))
       (should-not result)))))

(ert-deftest agent-shell-codex-local-index-uses-newest-schema-and-name-test ()
  "The highest schema number wins, and renamed threads keep their titles."
  (agent-shell-codex-test--with-index
   (lambda (directory db)
     (sqlite-execute db "ALTER TABLE threads ADD COLUMN name TEXT")
     (sqlite-execute db "ALTER TABLE threads ADD COLUMN preview TEXT")
     (sqlite-execute db "INSERT INTO threads (id,cwd,title,updated_at,archived,source,model_provider,name,preview) VALUES ('named', ?, 'Old title', 1, 0, 'cli', 'local', 'Renamed', 'Preview')"
                     (list directory))
     (copy-file (expand-file-name "state_5.sqlite" directory)
                (expand-file-name "state_10.sqlite" directory))
     (sqlite-execute db "DELETE FROM threads")
     (should (equal (map-elt (cadr (agent-shell-codex--local-sessions directory)) 'title)
                    "Renamed")))))

(ert-deftest agent-shell-codex-local-index-missing-falls-back-test ()
  "A missing or incompatible database preserves normal ACP discovery."
  (agent-shell-codex-test--with-index
   (lambda (directory db)
     (sqlite-execute db "DROP TABLE threads")
     (let ((called nil))
       (agent-shell-codex--list-sessions-a
        (lambda (&rest _) (setq called t))
        :state '((:agent-config . ((:identifier . codex)))) :cwd directory)
       (should called))
     (let ((agent-shell-codex-session-directory
            (expand-file-name "missing" directory)))
       (should-not (agent-shell-codex--local-sessions directory))))))

(ert-deftest agent-shell-codex-local-index-other-agents-use-acp-test ()
  "The opt-in local index does not intercept other agents or TRAMP."
  (let ((agent-shell-codex-session-directory "/tmp")
        (called nil))
    (cl-letf (((symbol-function 'agent-shell-codex--local-sessions)
               (lambda (_) (ert-fail "Other agent consulted Codex index"))))
      (agent-shell-codex--list-sessions-a
       (lambda (&rest _) (setq called t))
       :state '((:agent-config . ((:identifier . openclaw)))) :cwd "/tmp"))
    (should called)
    (should-not (agent-shell-codex--local-sessions "/ssh:host:/project/"))))

(ert-deftest agent-shell-codex-local-index-is-read-only-test ()
  "Session discovery opens the user's database in read-only mode."
  (agent-shell-codex-test--with-index
   (lambda (directory _db)
     (let ((open (symbol-function 'sqlite-open))
           (readonly nil))
       (cl-letf (((symbol-function 'sqlite-open)
                  (lambda (path &optional mode &rest _)
                    (setq readonly mode)
                    (funcall open path mode))))
         (should (agent-shell-codex--local-sessions directory)))
       (should readonly)))))

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
