;;; agent-shell-math.el --- LaTeX previews for agent-shell markdown -*- lexical-binding: t; -*-

;; Local extension.  Render delimited LaTeX snippets in agent-shell buffers by
;; reusing Org's LaTeX preview pipeline.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'org)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-markdown)

(defgroup agent-shell-math nil
  "Render LaTeX math snippets in agent-shell buffers."
  :group 'agent-shell)

(defcustom agent-shell-math-preview-enabled t
  "When non-nil, render LaTeX math snippets in agent-shell markdown output."
  :type 'boolean
  :group 'agent-shell-math)

(defcustom agent-shell-math-preview-process 'dvisvgm
  "Org LaTeX preview process used for agent-shell math previews."
  :type 'symbol
  :group 'agent-shell-math)

(defcustom agent-shell-math-preview-scale 0.75
  "Scale used for generated math previews."
  :type 'number
  :group 'agent-shell-math)

(defcustom agent-shell-math-preview-foreground nil
  "Foreground color for math previews.
When nil, use the current `default' face foreground."
  :type '(choice (const :tag "Default face foreground" nil) string)
  :group 'agent-shell-math)

(defcustom agent-shell-math-preview-background "Transparent"
  "Background color for math previews."
  :type 'string
  :group 'agent-shell-math)

(defcustom agent-shell-math-preview-cache-directory
  (expand-file-name "agent-shell/math/" user-emacs-directory)
  "Directory where generated math preview images are cached."
  :type 'directory
  :group 'agent-shell-math)

(defconst agent-shell-math--display-math-pattern
  (concat "\\(?:"
          (regexp-quote "\\[") "\\(?:.\\|\n\\)*?" (regexp-quote "\\]")
          "\\|"
          (regexp-quote "$$") "\\(?:.\\|\n\\)*?" (regexp-quote "$$")
          "\\)"))

(defconst agent-shell-math--inline-math-pattern
  (concat (regexp-quote "\\(") "[^\n]+?" (regexp-quote "\\)")))

(defconst agent-shell-math--dcp-message-id-pattern
  "^[[:blank:]]*<dcp-message-id>[^\n]*</dcp-message-id>[[:blank:]]*\n?"
  "Pattern for environment metadata lines that should not be displayed.")

(defconst agent-shell-math--metadata-line-pattern
  (concat "^[[:blank:]]*"
          "<" "dcp-message-id" ">"
          "[^\n]*"
          "</" "dcp-message-id" ">"
          "[[:blank:]]*\n?")
  "Pattern for hidden environment metadata lines.")

(defun agent-shell-math--face-contains-p (needle faces)
  "Return non-nil when NEEDLE appears anywhere in FACES."
  (cond
   ((eq needle faces) t)
   ((consp faces)
    (seq-some (lambda (face)
                (agent-shell-math--face-contains-p needle face))
              faces))
   (t nil)))

(defun agent-shell-math--protected-p (pos)
  "Return non-nil when POS is inside content that should not render math."
  (let ((faces (get-text-property pos 'face)))
    (or (get-text-property pos 'agent-shell-markdown-frozen)
        (agent-shell-math--face-contains-p 'agent-shell-markdown-inline-code faces)
        (agent-shell-math--face-contains-p 'agent-shell-markdown-source-block faces))))

(defun agent-shell-math--foreground ()
  "Return foreground color for previews."
  (let ((foreground (or agent-shell-math-preview-foreground
                        (face-foreground 'default nil t))))
    (if (and (stringp foreground)
             (not (string-empty-p foreground))
             (not (string-prefix-p "unspecified" foreground)))
        foreground
      "White")))

(defun agent-shell-math--options ()
  "Return options plist for `org-create-formula-image'."
  (let ((foreground (agent-shell-math--foreground))
        (background agent-shell-math-preview-background))
    (list :foreground foreground
          :background background
          :scale agent-shell-math-preview-scale
          :html-foreground foreground
          :html-background background
          :html-scale agent-shell-math-preview-scale
          :matchers '("begin" "$1" "$" "$$" "\\(" "\\["))))

(defun agent-shell-math-render-to-file (source)
  "Render delimited LaTeX SOURCE to an SVG file and return its path."
  (make-directory agent-shell-math-preview-cache-directory t)
  (let* ((key (secure-hash
               'sha1
               (prin1-to-string
                (list source
                      (agent-shell-math--foreground)
                      agent-shell-math-preview-background
                      agent-shell-math-preview-scale
                      agent-shell-math-preview-process))))
         (file (expand-file-name (concat key ".svg")
                                 agent-shell-math-preview-cache-directory)))
    (unless (file-exists-p file)
      ;; `org-create-formula-image' internally uses Org helpers that expect the
      ;; current buffer to derive from `org-mode'.  Running it directly from an
      ;; `agent-shell-mode' buffer emits repeated `org-element-at-point' warnings
      ;; while agent output is rendered.  Use a tiny temporary Org buffer as the
      ;; rendering context, but keep BUFFER nil because this is a standalone
      ;; image generation path, not an Org buffer preview update.
      (with-temp-buffer
        (delay-mode-hooks
          (org-mode))
        (insert source)
        (goto-char (point-min))
        (let ((org-preview-latex-default-process agent-shell-math-preview-process))
          (org-create-formula-image source
                                    file
                                    (agent-shell-math--options)
                                    nil
                                    agent-shell-math-preview-process))))
    file))

(defun agent-shell-math--preview-source (beg end)
  "Render LaTeX snippet between BEG and END as an inline image."
  (unless (or (get-text-property beg 'agent-shell-math-preview)
              (agent-shell-math--protected-p beg)
              (not (display-images-p))
              (not (image-type-available-p 'svg)))
    (let* ((source (buffer-substring-no-properties beg end))
           (file (agent-shell-math-render-to-file source))
           (image (create-image file 'svg nil :ascent 'center)))
      (add-text-properties beg end '(agent-shell-math-preview t))
      (put-text-property beg end 'rear-nonsticky
                          '(display invisible agent-shell-math-preview))
      (put-text-property beg (min (1+ beg) end) 'display image)
      (when (< (1+ beg) end)
        ;; Do not rely on `invisible' here: agent-shell buffers may not have a
        ;; matching `buffer-invisibility-spec'.  Empty display text reliably
        ;; suppresses raw LaTeX remnants after the preview image.
        (put-text-property (1+ beg) end 'display "")
        (put-text-property (1+ beg) end 'invisible t)))))

(defun agent-shell-math--hide-metadata-lines ()
  "Hide metadata lines that can leak from API/debug output."
  (goto-char (point-min))
  (while (re-search-forward agent-shell-math--metadata-line-pattern nil t)
    (let ((beg (match-beginning 0))
          (end (match-end 0)))
      (put-text-property beg end 'display "")
      (put-text-property beg end 'rear-nonsticky '(display)))))

(defun agent-shell-math--render-pattern (pattern end avoid-ranges)
  "Render all LaTeX snippets matching PATTERN before END.
Skip matches inside AVOID-RANGES, such as Markdown fenced code blocks."
  (goto-char (point-min))
  (while (re-search-forward pattern end t)
    (let ((beg (match-beginning 0))
          (finish (match-end 0)))
      (unless (agent-shell-markdown-in-avoid-range-p beg finish avoid-ranges)
        (condition-case err
            (agent-shell-math--preview-source beg finish)
          (error
           (message "agent-shell math preview failed: %s" (error-message-string err)))))
      (goto-char finish))))

(defun agent-shell-math--source-block-avoid-ranges ()
  "Return Markdown fenced source-block ranges for math rendering."
  (agent-shell-markdown-sort-ranges
   (mapcar (lambda (source-block)
             (cons (map-nested-elt source-block '(:block :start))
                   (map-nested-elt source-block '(:block :end))))
           (agent-shell-markdown--source-blocks))))

(defun agent-shell-math-preview-region (beg end)
  "Render LaTeX math previews between BEG and END in the current buffer."
  (when agent-shell-math-preview-enabled
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (let ((avoid-ranges (agent-shell-math--source-block-avoid-ranges)))
          (agent-shell-math--render-pattern
           agent-shell-math--display-math-pattern (point-max) avoid-ranges)
          (agent-shell-math--render-pattern
           agent-shell-math--inline-math-pattern (point-max) avoid-ranges))))))

(cl-defun agent-shell-math-render-markdown (&key (render-images t)
                                                 (highlight-blocks t)
                                                 image-cache-directory
                                                 &allow-other-keys)
  "Render agent-shell Markdown, then render LaTeX math snippets.
Accepts the same keyword arguments as `agent-shell-markdown-render-function'."
  (agent-shell-markdown-replace-markup
   :render-images render-images
   :highlight-blocks highlight-blocks
   :image-cache-directory image-cache-directory)
  (agent-shell-math--hide-metadata-lines)
  (agent-shell-math-preview-region (point-min) (point-max)))

(provide 'agent-shell-math)

;;; agent-shell-math.el ends here
