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

(defcustom agent-shell-math-latex-command
  (or (executable-find "latex") "latex")
  "LaTeX compiler used for complete fenced LaTeX blocks.

The fenced renderer uses the DVI output because dvisvgm produces SVG that
Emacs can display reliably."
  :type 'file
  :group 'agent-shell-math)

(defcustom agent-shell-math-dvi-svg-command
  (or (executable-find "dvisvgm") "dvisvgm")
  "DVI-to-SVG converter used for complete fenced LaTeX blocks."
  :type 'file
  :group 'agent-shell-math)

(defcustom agent-shell-math-preview-max-width 0.9
  "Maximum width of a rendered fenced LaTeX block as a window fraction."
  :type 'number
  :group 'agent-shell-math)

(defconst agent-shell-math--fenced-languages
  '("latex" "tex" "tikz" "tikzpicture" "pgfplot" "pgfplots")
  "Markdown fence languages rendered as complete LaTeX previews.")

(defface agent-shell-math-header
  '((t :inherit shadow :height 0.9))
  "Face for the action hint above a rendered LaTeX block."
  :group 'agent-shell-math)

(defface agent-shell-math-pending
  '((t :inherit shadow :slant italic))
  "Face for a LaTeX block that is still streaming."
  :group 'agent-shell-math)

(defface agent-shell-math-error
  '((t :inherit error))
  "Face for a LaTeX rendering error."
  :group 'agent-shell-math)

(defvar agent-shell-math-preview-map nil
  "Keymap active on rendered fenced LaTeX previews.")

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

(defun agent-shell-math--fenced-language-p (language)
  "Return non-nil when LANGUAGE names a renderable LaTeX fence."
  (member (downcase language) agent-shell-math--fenced-languages))

(defun agent-shell-math--full-document-p (source)
  "Return non-nil when SOURCE already contains a complete LaTeX document."
  (or (string-match-p "\\\\documentclass" source)
      (string-match-p "\\\\begin{document}" source)))

(defun agent-shell-math--document-source (source)
  "Return a compilable LaTeX document containing SOURCE.

Complete documents are passed through unchanged.  Fragments are wrapped in a
standalone document suitable for equations, TikZ, and PGFPlots."
  (if (agent-shell-math--full-document-p source)
      source
    (let ((needs-pgfplots
           (or (string-match-p "\\\\begin{axis}" source)
               (string-match-p "\\\\addplot" source)
               (string-match-p "\\\\pgfplotsset" source)
               (string-match-p "\\\\usepackage{pgfplots}" source)))
          (needs-tikz
           (or (string-match-p "\\\\begin{tikzpicture}" source)
               (string-match-p "\\\\(?:draw\\|path\\|node\\|fill\\|shade)\\b"
                               source)
               (string-match-p "\\\\usepackage{tikz}" source)))
          (packages "\\usepackage{amsmath,amssymb}\n"))
      (when needs-tikz
        (setq packages (concat packages "\\usepackage{tikz}\n")))
      (when needs-pgfplots
        (setq packages
              (concat packages
                      "\\usepackage{pgfplots}\n"
                      "\\pgfplotsset{compat=1.18}\n")))
      (format "\\documentclass[tikz,border=5pt]{standalone}
%s\\begin{document}
%s
\\end{document}
"
              packages source))))

(defun agent-shell-math--diagnostic (text)
  "Return a bounded, useful portion of compiler diagnostic TEXT."
  (let ((text (string-trim text)))
    (if (> (length text) 2400)
        (concat "…" (substring text (- (length text) 2400)))
      text)))

(defun agent-shell-math-render-latex-to-file (source)
  "Compile fenced LaTeX SOURCE and return a cached SVG path.

SOURCE may be a complete LaTeX document or a TikZ/PGFPlots fragment.  The
fragment is wrapped in a standalone document before compilation.  The
document is compiled to DVI and converted with dvisvgm because its SVG
output is compatible with Emacs' native image renderer."
  (make-directory agent-shell-math-preview-cache-directory t)
  (let* ((document (agent-shell-math--document-source source))
         (key (secure-hash
               'sha256
               (prin1-to-string
                (list "agent-shell-math-fenced-v1"
                      document
                      agent-shell-math-latex-command
                      agent-shell-math-dvi-svg-command))))
         (file (expand-file-name (concat key ".svg")
                                 agent-shell-math-preview-cache-directory)))
    (unless (file-exists-p file)
      (let* ((directory (make-temp-file "agent-shell-latex-" t))
             (tex-file (expand-file-name "main.tex" directory))
             (dvi-file (expand-file-name "main.dvi" directory))
             (svg-base (expand-file-name "preview" directory))
             (svg-file (concat svg-base ".svg"))
             diagnostic)
        (unwind-protect
            (progn
              (with-temp-file tex-file
                (insert document))
              (with-temp-buffer
                (let ((status
                       (process-file agent-shell-math-latex-command
                                     nil t nil
                                     "-interaction=nonstopmode"
                                     "-halt-on-error"
                                     "-file-line-error"
                                     "-no-shell-escape"
                                     "-output-directory" directory
                                     tex-file)))
                  (setq diagnostic (buffer-string))
                  (unless (and (integerp status) (zerop status)
                               (file-exists-p dvi-file))
                    (error "LaTeX compilation failed (%s): %s"
                           status (agent-shell-math--diagnostic diagnostic)))))
              (with-temp-buffer
                (let ((status
                       (process-file agent-shell-math-dvi-svg-command
                                     nil t nil
                                     "-n"
                                     "--no-styles"
                                     "-o" svg-file dvi-file)))
                  (setq diagnostic (buffer-string))
                  (unless (and (integerp status) (zerop status)
                               (file-exists-p svg-file)
                               (> (file-attribute-size
                                   (file-attributes svg-file)) 0))
                    (error "DVI to SVG conversion failed (%s): %s"
                           status (agent-shell-math--diagnostic diagnostic)))))
              (rename-file svg-file file t))
          (when (file-directory-p directory)
            (delete-directory directory t)))))
    file))

(defun agent-shell-math--preview-max-width ()
  "Return the maximum pixel width for a fenced LaTeX preview."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 1
         (floor (* (or (and window (window-body-width window t)) 900)
                   agent-shell-math-preview-max-width)))))

(defun agent-shell-math--source-at-point ()
  "Return fenced LaTeX source stored at point, or nil."
  (get-text-property (point) 'agent-shell-math-source))

(defun agent-shell-math-open-at-point (&optional event)
  "Open the rendered LaTeX SVG at point or mouse EVENT."
  (interactive (list last-nonmenu-event))
  (when event
    (mouse-set-point event))
  (if-let* ((file (get-text-property (point) 'agent-shell-math-file)))
      (find-file-other-window file)
    (user-error "No LaTeX preview at point")))

(defun agent-shell-math-copy-source-at-point ()
  "Copy fenced LaTeX source stored at point."
  (interactive)
  (if-let* ((source (agent-shell-math--source-at-point)))
      (progn
        (kill-new source)
        (message "Copied LaTeX source"))
    (user-error "No LaTeX source at point")))

(defun agent-shell-math-view-source-at-point ()
  "Show fenced LaTeX source stored at point in another window."
  (interactive)
  (if-let* ((source (agent-shell-math--source-at-point)))
      (let ((buffer (get-buffer-create "*agent-shell LaTeX source*")))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert source)
            (goto-char (point-min))
            (if (fboundp 'latex-mode)
                (latex-mode)
              (fundamental-mode))
            (view-mode 1)))
        (pop-to-buffer buffer))
    (user-error "No LaTeX source at point")))

(setq agent-shell-math-preview-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "RET") #'agent-shell-math-open-at-point)
        (define-key map (kbd "v") #'agent-shell-math-view-source-at-point)
        (define-key map (kbd "c") #'agent-shell-math-copy-source-at-point)
        (define-key map [mouse-1] #'agent-shell-math-open-at-point)
        map))

(defun agent-shell-math--clear-overlays (beg end)
  "Delete LaTeX renderer overlays intersecting BEG..END."
  (dolist (overlay (overlays-in beg end))
    (when (overlay-get overlay 'agent-shell-math-overlay)
      (delete-overlay overlay))))

(defun agent-shell-math--clear-preview-properties (beg end)
  "Remove LaTeX preview properties from BEG..END."
  (with-silent-modifications
    (remove-text-properties
     beg end
     '(display nil
       agent-shell-math-preview nil
       agent-shell-math-file nil
       agent-shell-math-source nil
       agent-shell-math-language nil
       agent-shell-markdown-frozen nil
       agent-shell-markdown-source nil
       help-echo nil keymap nil pointer nil))))

(defun agent-shell-math--pending-overlay (beg end)
  "Show a streaming placeholder over the LaTeX block BEG..END."
  (let ((overlay (make-overlay beg end nil nil t)))
    (overlay-put overlay 'agent-shell-math-overlay 'pending)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'display
                 (propertize
                  "LaTeX preview... waiting for closing fence"
                  'face 'agent-shell-math-pending))
    overlay))

(defun agent-shell-math--error-overlay (beg message)
  "Show LaTeX rendering error MESSAGE before BEG."
  (let ((overlay (make-overlay beg beg)))
    (overlay-put overlay 'agent-shell-math-overlay 'error)
    (overlay-put overlay 'before-string
                 (propertize
                  (format "LaTeX preview failed: %s\n" message)
                  'face 'agent-shell-math-error))))

(defun agent-shell-math--install-preview
    (beg end body language file markdown)
  "Install FILE as the fenced LaTeX preview for BEG..END."
  (let ((image (create-image file 'svg nil :ascent 'center
                             :max-width (agent-shell-math--preview-max-width)))
        (header (propertize
                 (format "%s  RET open · v source · c copy\n" language)
                 'face 'agent-shell-math-header)))
    (when (display-graphic-p)
      (image-flush image))
    (with-silent-modifications
      (agent-shell-math--clear-preview-properties beg end)
      (add-text-properties
       beg end
       (list 'agent-shell-math-preview t
             'agent-shell-math-file file
             'agent-shell-math-source body
             'agent-shell-math-language language
             'agent-shell-markdown-frozen t
             'agent-shell-markdown-source markdown
             'help-echo "RET opens image; v views source; c copies source"
             'keymap agent-shell-math-preview-map
             'pointer 'hand
             'rear-nonsticky
             '(display agent-shell-math-preview agent-shell-math-file
                       agent-shell-math-source agent-shell-math-language
                       agent-shell-markdown-frozen agent-shell-markdown-source
                       help-echo keymap pointer)))
      (put-text-property beg (1+ beg) 'display header)
      (put-text-property (1+ beg) (+ beg 2) 'display image)
      (put-text-property (+ beg 2) (+ beg 3) 'display "\n")
      (when (< (+ beg 3) end)
        (put-text-property (+ beg 3) end 'display "")))))

(defun agent-shell-math--preview-fenced-block (source-block)
  "Render one complete LaTeX SOURCE-BLOCK, or hold it while streaming."
  (let* ((language (map-elt source-block :language))
         (beg (map-nested-elt source-block '(:block :start)))
         (end (map-nested-elt source-block '(:block :end)))
         (body (map-elt source-block :body))
         (complete (map-elt source-block :complete)))
    (when (agent-shell-math--fenced-language-p language)
      (if (not complete)
          (progn
            (agent-shell-math--clear-overlays beg end)
            (agent-shell-math--clear-preview-properties beg end)
            (agent-shell-math--pending-overlay beg end)
            'pending)
        (let ((markdown (buffer-substring-no-properties beg end)))
          (unless (and (equal body (get-text-property
                                    beg 'agent-shell-math-source))
                       (get-text-property beg 'agent-shell-math-preview))
            (agent-shell-math--clear-overlays beg end)
            (when (and (display-images-p)
                       (image-type-available-p 'svg))
              (agent-shell-math--install-preview
               beg end body language
               (agent-shell-math-render-latex-to-file body)
               markdown))))
        'rendered))))

(defun agent-shell-math-preview-fenced-blocks (source-blocks)
  "Render supported fenced LaTeX blocks from SOURCE-BLOCKS."
  (let (watermarks)
    (when agent-shell-math-preview-enabled
      (dolist (source-block source-blocks)
        (when (agent-shell-math--fenced-language-p
               (map-elt source-block :language))
          (condition-case err
              (when (eq (agent-shell-math--preview-fenced-block source-block)
                        'pending)
                (push (map-nested-elt source-block '(:block :start))
                      watermarks))
            (error
             (let ((beg (map-nested-elt source-block '(:block :start)))
                   (end (map-nested-elt source-block '(:block :end))))
               (agent-shell-math--clear-overlays beg end)
               (agent-shell-math--clear-preview-properties beg end)
               (agent-shell-math--error-overlay
                beg (error-message-string err))))))))
    (when watermarks
      (list (cons :watermark (apply #'min watermarks))))))

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
      (add-text-properties
       beg end
       (list 'agent-shell-math-preview t
             'agent-shell-markdown-frozen t
             'agent-shell-markdown-source source))
      (put-text-property beg end 'rear-nonsticky
                          '(display invisible agent-shell-math-preview
                            agent-shell-markdown-frozen
                            agent-shell-markdown-source))
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
           ;; Keep valid LaTeX intact if image generation fails.
           (add-text-properties
            beg finish
            (list 'agent-shell-markdown-frozen t
                  'agent-shell-markdown-source
                  (buffer-substring-no-properties beg finish)))
           (message "agent-shell math preview failed: %s"
                    (error-message-string err)))))
      (goto-char finish))))

(defun agent-shell-math--context-avoid-ranges (context)
  "Return code ranges from agent-shell Markdown render CONTEXT."
  (agent-shell-markdown-sort-ranges
   (append
    (map-elt context :inline-code-ranges)
    (mapcar (lambda (source-block)
              (cons (map-nested-elt source-block '(:block :start))
                    (map-nested-elt source-block '(:block :end))))
            (map-elt context :source-blocks)))))

(defun agent-shell-math--unclosed-delimiter-start (avoid-ranges)
  "Return the earliest unclosed math delimiter outside AVOID-RANGES."
  (let (earliest)
    (dolist (spec '(("\\[" . "\\]") ("$$" . "$$") ("\\(" . "\\)")))
      (save-excursion
        (goto-char (point-min))
        (let ((opener (regexp-quote (car spec)))
              (closer (regexp-quote (cdr spec))))
          (while (re-search-forward opener nil t)
            (let ((beg (match-beginning 0)))
              (cond
               ((or (agent-shell-math--protected-p beg)
                    (agent-shell-markdown-in-avoid-range-p
                     beg (min (1+ beg) (point-max)) avoid-ranges)))
               ((let ((close-end
                       (save-excursion
                         (when (re-search-forward closer nil t)
                           (match-end 0)))))
                  (if close-end (progn (goto-char close-end) t) nil)))
               (t
                (setq earliest (if earliest (min earliest beg) beg))
                (goto-char (point-max)))))))))
    earliest))

(defun agent-shell-math--clear-pending ()
  "Unfreeze math held pending from an earlier streaming pass."
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-property-change
                   pos 'agent-shell-math-pending nil (point-max))))
        (when (get-text-property pos 'agent-shell-math-pending)
          (remove-list-of-text-properties
           pos next
           '(agent-shell-math-pending agent-shell-markdown-frozen)))
        (setq pos next)))))

(defun agent-shell-math--mark-pending (beg end)
  "Protect incomplete math between BEG and END until more text arrives."
  (add-text-properties
   beg end
   '(agent-shell-math-pending t agent-shell-markdown-frozen t))
  ;; Text appended by the next streaming chunk must remain unfrozen so the
  ;; complete expression can be reconsidered from the saved watermark.
  (put-text-property
   beg end 'rear-nonsticky
   '(agent-shell-math-pending agent-shell-markdown-frozen)))

(defun agent-shell-math-render-context (context)
  "Render fenced LaTeX blocks and delimited math in CONTEXT."
  (when agent-shell-math-preview-enabled
    (save-excursion
      (agent-shell-math--clear-pending)
      (let ((fenced-result
             (agent-shell-math-preview-fenced-blocks
              (map-elt context :source-blocks)))
            (avoid-ranges (agent-shell-math--context-avoid-ranges context)))
        (agent-shell-math--render-pattern
         agent-shell-math--display-math-pattern (point-max) avoid-ranges)
        (agent-shell-math--render-pattern
         agent-shell-math--inline-math-pattern (point-max) avoid-ranges)
        (when-let* ((watermark
                     (agent-shell-math--unclosed-delimiter-start avoid-ranges)))
          (agent-shell-math--mark-pending watermark (point-max))
          (setq fenced-result
                (cons (cons :watermark watermark) fenced-result)))
        fenced-result))))

(defun agent-shell-math-preview-region (beg end)
  "Render LaTeX math previews between BEG and END in the current buffer."
  (when agent-shell-math-preview-enabled
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (agent-shell-math-render-context (agent-shell-markdown-context))))))

(cl-defun agent-shell-math-render-markdown (&key (render-images t)
                                                 (highlight-blocks t)
                                                 image-cache-directory
                                                 &allow-other-keys)
  "Render agent-shell Markdown, then render LaTeX math snippets.
Accepts the same keyword arguments as `agent-shell-markdown-render-function'."
  ;; agent-shell 0.73+ decodes escapes during built-in Markdown passes.
  ;; Claim math through the pre-render hook before those passes run.
  (let ((agent-shell-markdown-render-functions
         (if (and render-images
                  (not (memq #'agent-shell-math-render-context
                             agent-shell-markdown-render-functions)))
             (cons #'agent-shell-math-render-context
                   agent-shell-markdown-render-functions)
           agent-shell-markdown-render-functions)))
    (agent-shell-markdown-replace-markup
     :render-images render-images
     :highlight-blocks highlight-blocks
     :image-cache-directory image-cache-directory))
  (agent-shell-math--hide-metadata-lines))

;; Register directly with the Markdown pipeline so previews remain active when
;; users keep agent-shell's standard Markdown renderer configured.
(add-hook 'agent-shell-markdown-render-functions
          #'agent-shell-math-render-context)

(provide 'agent-shell-math)

;;; agent-shell-math.el ends here
