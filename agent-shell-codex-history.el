;;; agent-shell-codex-history.el --- History compatibility -*- lexical-binding: t; -*-

;;; Commentary:
;; Compatibility entry points for the shared history reader.

;;; Code:
(require 'agent-shell-history)

(defun agent-shell-codex-history--migrate-buffers ()
  "Move existing Codex readers to the shared history state.
Emacs cannot turn an automatically buffer-local variable into an alias."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (dolist (name '(page source session-id pages index loading error request-token))
        (let ((old (intern (format "agent-shell-codex-history--%s" name)))
              (new (intern (format "agent-shell-history--%s" name))))
          (when (and (local-variable-p old) (not (local-variable-p new)))
            (set (make-local-variable new) (symbol-value old))))))))

(agent-shell-codex-history--migrate-buffers)
(advice-remove 'agent-shell--send-request #'agent-shell-codex-history--send-request-a)
(defvaralias 'agent-shell-codex-history--meta-key 'agent-shell-history--meta-key)
(defvaralias 'agent-shell-codex-history-mode-map 'agent-shell-history-mode-map)
(defalias 'agent-shell-codex-history--notification #'agent-shell-history--notification)
(defalias 'agent-shell-codex-history--send-request-a #'agent-shell-history--send-request-a)
(defalias 'agent-shell-codex-history--insert-link #'agent-shell-history--insert-link)
(defalias 'agent-shell-codex-history-mode #'agent-shell-history-mode)
(defalias 'agent-shell-codex-history-open #'agent-shell-history-open)
(defalias 'agent-shell-codex-history--content #'agent-shell-history--content)
(defalias 'agent-shell-codex-history--update-text #'agent-shell-history--update-text)
(defalias 'agent-shell-codex-history--literal #'agent-shell-history--literal)
(defalias 'agent-shell-codex-history--render-page #'agent-shell-history--render-page)
(defalias 'agent-shell-codex-history-newer #'agent-shell-history-newer)
(defalias 'agent-shell-codex-history-older #'agent-shell-history-older)
(defalias 'agent-shell-codex-history--fetch-older #'agent-shell-history--fetch-older)

(provide 'agent-shell-codex-history)
;;; agent-shell-codex-history.el ends here
