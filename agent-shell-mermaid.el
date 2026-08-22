;;; agent-shell-mermaid.el --- Mermaid previews for agent-shell markdown -*- lexical-binding: t; -*-

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-markdown)

(defgroup agent-shell-mermaid nil
  "Render Mermaid diagrams in agent-shell buffers."
  :group 'agent-shell)

(defcustom agent-shell-mermaid-preview-enabled t
  "When non-nil, render complete Mermaid fenced blocks as inline SVG images."
  :type 'boolean :group 'agent-shell-mermaid)

(defcustom agent-shell-mermaid-mmdc-command nil
  "Path to Mermaid CLI, or nil to find mmdc on PATH."
  :type '(choice (const :tag "Find mmdc on PATH" nil) file)
  :group 'agent-shell-mermaid)

(defcustom agent-shell-mermaid-browser-executable nil
  "Browser executable used by Mermaid CLI through Puppeteer."
  :type '(choice (const :tag "Auto-detect browser" nil) file)
  :group 'agent-shell-mermaid)

(defcustom agent-shell-mermaid-theme 'auto
  "Mermaid theme, or auto to follow the frame background mode."
  :type '(choice (const auto) (const default) (const forest)
                 (const dark) (const neutral))
  :group 'agent-shell-mermaid)

(defcustom agent-shell-mermaid-background "transparent"
  "Background color passed to Mermaid CLI."
  :type 'string :group 'agent-shell-mermaid)

(defcustom agent-shell-mermaid-max-width 0.9
  "Maximum preview width as a fraction of its window width."
  :type 'number :group 'agent-shell-mermaid)

(defcustom agent-shell-mermaid-cache-directory
  (expand-file-name "agent-shell/mermaid/" user-emacs-directory)
  "Directory where generated Mermaid SVG previews are cached."
  :type 'directory :group 'agent-shell-mermaid)

(defvar agent-shell-mermaid-debug nil
  "When non-nil, log Mermaid rendering diagnostics to *Messages*.")

(defvar agent-shell-mermaid--id-counter 0
  "Counter used to keep SVG ids unique between rendered blocks.")

(defface agent-shell-mermaid-header
  '((t :inherit shadow :height 0.9))
  "Face for the action hint above a Mermaid preview."
  :group 'agent-shell-mermaid)

(defface agent-shell-mermaid-pending
  '((t :inherit shadow :slant italic))
  "Face for a Mermaid block that is still streaming."
  :group 'agent-shell-mermaid)

(defface agent-shell-mermaid-error
  '((t :inherit error))
  "Face for a Mermaid rendering error."
  :group 'agent-shell-mermaid)

(defvar agent-shell-mermaid-preview-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-shell-mermaid-open-at-point)
    (define-key map (kbd "v") #'agent-shell-mermaid-view-source-at-point)
    (define-key map (kbd "c") #'agent-shell-mermaid-copy-source-at-point)
    (define-key map [mouse-1] #'agent-shell-mermaid-open-at-point)
    map)
  "Keymap active on rendered Mermaid previews.")

(defun agent-shell-mermaid--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS when Mermaid debugging is enabled."
  (when agent-shell-mermaid-debug
    (apply #'message (concat "agent-shell Mermaid: " format-string) args)))

(defun agent-shell-mermaid--mmdc-command ()
  "Return an executable Mermaid CLI path, or nil."
  (let ((configured agent-shell-mermaid-mmdc-command))
    (cond
     ((and configured (file-executable-p configured)) configured)
     ((null configured) (executable-find "mmdc"))
     (t nil))))

(defun agent-shell-mermaid--browser-candidates ()
  "Return possible Puppeteer browser executables in preference order."
  (append
   (delq nil (list agent-shell-mermaid-browser-executable
                   (getenv "PUPPETEER_EXECUTABLE_PATH")
                   (executable-find "chromium")
                   (executable-find "chromium-browser")
                   (executable-find "google-chrome")
                   (executable-find "google-chrome-stable")))
   (reverse
    (sort (file-expand-wildcards
           (expand-file-name ".agent-browser/browsers/chrome-*/chrome" "~"))
          #'string-lessp))
   (seq-filter #'file-executable-p
               '("/opt/brave.com/brave/brave-browser"
                 "/opt/brave.com/brave-beta/brave-browser-beta"))))

(defun agent-shell-mermaid--browser-executable ()
  "Return the first executable Puppeteer browser candidate, or nil."
  (seq-find #'file-executable-p (agent-shell-mermaid--browser-candidates)))

(defun agent-shell-mermaid--display-frame ()
  "Return the frame displaying the current buffer, or the selected frame."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (window-frame window)
    (selected-frame)))

(defun agent-shell-mermaid--theme ()
  "Return the Mermaid CLI theme name for the current buffer."
  (symbol-name
   (if (eq agent-shell-mermaid-theme 'auto)
       (if (eq (or (frame-parameter (agent-shell-mermaid--display-frame)
                                    'background-mode)
                   frame-background-mode)
               'dark)
           'dark
         'default)
     agent-shell-mermaid-theme)))

(defun agent-shell-mermaid--preview-max-width ()
  "Return the maximum preview width in pixels."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 1 (floor (* (if window (window-body-width window t)
                       (window-body-width nil t))
                     agent-shell-mermaid-max-width)))))

(defun agent-shell-mermaid--next-id (source)
  "Return a unique, SVG-safe id for Mermaid SOURCE."
  (setq agent-shell-mermaid--id-counter (1+ agent-shell-mermaid--id-counter))
  (format "agent-shell-mermaid-%s-%d"
          (substring (secure-hash 'sha256 source) 0 12)
          agent-shell-mermaid--id-counter))

(defun agent-shell-mermaid--validate-svg (file)
  "Signal an error unless FILE is a safe SVG supported by Emacs."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (let ((case-fold-search t))
      (goto-char (point-min))
      (unless (re-search-forward "<svg\\(?:[[:space:]>]\\)" nil t)
        (error "Mermaid CLI did not produce an SVG"))
      (goto-char (point-min))
      (when (re-search-forward
             "<\\(?:script\\|foreignObject\\)\\b\\|\\bon[a-z]+[[:space:]]*=\\|\\(?:href\\|xlink:href\\)[[:space:]]*=[[:space:]]*[\"']\\(?:javascript\\|data\\):"
             nil t)
        (error "Mermaid SVG contains unsupported or unsafe markup: %s"
               (match-string-no-properties 0))))))

(defun agent-shell-mermaid-render-to-file (source &optional svg-id theme)
  "Render Mermaid SOURCE to a cached SVG file and return its path."
  (let ((mmdc (or (agent-shell-mermaid--mmdc-command)
                  (error "Mermaid CLI not found; install @mermaid-js/mermaid-cli")))
        (browser (agent-shell-mermaid--browser-executable))
        (theme (or theme (agent-shell-mermaid--theme)))
        (svg-id (or svg-id (agent-shell-mermaid--next-id source))))
    (make-directory agent-shell-mermaid-cache-directory t)
    (let* ((key (secure-hash
                 'sha256
                 (prin1-to-string
                  (list "agent-shell-mermaid-v2" source svg-id theme
                        agent-shell-mermaid-background mmdc browser))))
           (file (expand-file-name (concat key ".svg")
                                   agent-shell-mermaid-cache-directory)))
      (unless (file-exists-p file)
        (let ((input (make-temp-file "agent-shell-mermaid-" nil ".mmd" source))
              (config (make-temp-file "agent-shell-mermaid-" nil ".json"))
              (output (make-temp-file
                       (expand-file-name ".agent-shell-mermaid-"
                                         agent-shell-mermaid-cache-directory)
                       nil ".svg"))
              (process-environment (copy-sequence process-environment))
              (started (float-time)))
          (unwind-protect
              (progn
                (with-temp-file config
                  (insert "{\"securityLevel\":\"strict\",\"htmlLabels\":false,\"flowchart\":{\"htmlLabels\":false}}"))
                (when browser (setenv "PUPPETEER_EXECUTABLE_PATH" browser))
                (with-temp-buffer
                  (let ((status (process-file
                                 mmdc nil t nil "-q" "-i" input "-o" output
                                 "-t" theme "-b" agent-shell-mermaid-background
                                 "-c" config "-I" svg-id)))
                    (unless (and (integerp status) (zerop status)
                                 (file-exists-p output)
                                 (> (file-attribute-size
                                     (file-attributes output)) 0))
                      (error "Mermaid CLI failed (%s): %s"
                             status (string-trim (buffer-string))))))
                (agent-shell-mermaid--validate-svg output)
                (rename-file output file t)
                (agent-shell-mermaid--log "rendered %s in %.2fs (%s theme)"
                                           svg-id (- (float-time) started) theme))
            (dolist (temp (list input config output))
              (when (file-exists-p temp) (delete-file temp))))))
      file)))

(defun agent-shell-mermaid--source-at-point ()
  "Return the Mermaid body stored at point, or nil."
  (get-text-property (point) 'agent-shell-mermaid-source))

(defun agent-shell-mermaid-open-at-point (&optional event)
  "Open the Mermaid SVG at point or mouse EVENT in image-mode."
  (interactive (list last-nonmenu-event))
  (when event (mouse-set-point event))
  (if-let* ((file (get-text-property (point) 'agent-shell-mermaid-file)))
      (find-file-other-window file)
    (user-error "No Mermaid preview at point")))

(defun agent-shell-mermaid-copy-source-at-point ()
  "Copy the Mermaid source stored at point."
  (interactive)
  (if-let* ((source (agent-shell-mermaid--source-at-point)))
      (progn (kill-new source) (message "Copied Mermaid source"))
    (user-error "No Mermaid source at point")))

(defun agent-shell-mermaid-view-source-at-point ()
  "Show the Mermaid source stored at point in another window."
  (interactive)
  (if-let* ((source (agent-shell-mermaid--source-at-point)))
      (let ((buffer (get-buffer-create "*agent-shell Mermaid source*")))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert source)
            (goto-char (point-min))
            (if (fboundp 'mermaid-mode) (mermaid-mode) (fundamental-mode))
            (view-mode 1)))
        (pop-to-buffer buffer))
    (user-error "No Mermaid source at point")))

(defun agent-shell-mermaid--clear-overlays (beg end)
  "Delete Mermaid-owned overlays intersecting BEG..END."
  (dolist (overlay (overlays-in beg end))
    (when (overlay-get overlay 'agent-shell-mermaid-overlay)
      (delete-overlay overlay))))

(defun agent-shell-mermaid--pending-overlay (beg end)
  "Display a streaming placeholder over the Mermaid block BEG..END."
  (let ((overlay (make-overlay beg end nil nil t)))
    (overlay-put overlay 'agent-shell-mermaid-overlay 'pending)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'display
                 (propertize "Mermaid diagram… waiting for closing fence"
                             'face 'agent-shell-mermaid-pending))))

(defun agent-shell-mermaid--error-overlay (beg message)
  "Show Mermaid rendering error MESSAGE before BEG."
  (let ((overlay (make-overlay beg beg)))
    (overlay-put overlay 'agent-shell-mermaid-overlay 'error)
    (overlay-put overlay 'before-string
                 (propertize (format "Mermaid render failed: %s\n" message)
                             'face 'agent-shell-mermaid-error))))

(defun agent-shell-mermaid--preview-block (source-block &optional force)
  "Render one Mermaid SOURCE-BLOCK, refreshing it when FORCE is non-nil."
  (when (equal (map-elt source-block :language) "mermaid")
    (let* ((beg (map-nested-elt source-block '(:block :start)))
           (end (map-nested-elt source-block '(:block :end)))
           (body (map-elt source-block :body))
           (complete (map-elt source-block :complete)))
      (agent-shell-mermaid--clear-overlays beg end)
      (if (not complete)
          (progn (agent-shell-mermaid--pending-overlay beg end) 'pending)
        (when (and (< (+ beg 2) end) (display-images-p)
                   (image-type-available-p 'svg))
          (let* ((theme (agent-shell-mermaid--theme))
                 (current-source (get-text-property beg 'agent-shell-mermaid-source))
                 (current-theme (get-text-property beg 'agent-shell-mermaid-theme)))
            (if (and (not force) (equal current-source body)
                     (equal current-theme theme)
                     (get-text-property beg 'agent-shell-mermaid-preview))
                'rendered
              (let* ((svg-id (or (get-text-property beg 'agent-shell-mermaid-id)
                                 (agent-shell-mermaid--next-id body)))
                     (file (agent-shell-mermaid-render-to-file body svg-id theme))
                     (image (create-image file 'svg nil :ascent 'center
                                          :max-width (agent-shell-mermaid--preview-max-width)))
                     (markdown (buffer-substring-no-properties beg end))
                     (header (propertize
                              "mermaid  RET open/zoom · v source · c copy\n"
                              'face 'agent-shell-mermaid-header)))
                (with-silent-modifications
                  (remove-text-properties beg end '(display nil))
                  (add-text-properties
                   beg end
                   (list 'agent-shell-mermaid-preview t
                         'agent-shell-mermaid-file file
                         'agent-shell-mermaid-id svg-id
                         'agent-shell-mermaid-source body
                         'agent-shell-mermaid-theme theme
                         'agent-shell-markdown-frozen t
                         'agent-shell-markdown-source markdown
                         'help-echo "RET opens image-mode; v views source; c copies source"
                         'keymap agent-shell-mermaid-preview-map
                         'pointer 'hand
                         'rear-nonsticky
                         '(display agent-shell-mermaid-preview
                                   agent-shell-mermaid-file agent-shell-mermaid-id
                                   agent-shell-mermaid-source agent-shell-mermaid-theme
                                   agent-shell-markdown-frozen agent-shell-markdown-source
                                   help-echo keymap pointer)))
                  (put-text-property beg (1+ beg) 'display header)
                  (put-text-property (1+ beg) (+ beg 2) 'display image)
                  (put-text-property (+ beg 2) (+ beg 3) 'display "\n")
                  (when (< (+ beg 3) end)
                    (put-text-property (+ beg 3) end 'display "")))
                'rendered))))))))

(defun agent-shell-mermaid-preview-blocks (source-blocks &optional force)
  "Render Mermaid blocks from SOURCE-BLOCKS, optionally with FORCE."
  (let ((watermarks '()))
    (when agent-shell-mermaid-preview-enabled
      (dolist (source-block source-blocks)
        (when (equal (map-elt source-block :language) "mermaid")
          (condition-case err
              (when (eq (agent-shell-mermaid--preview-block source-block force)
                        'pending)
                (push (map-nested-elt source-block '(:block :start)) watermarks))
            (error
             (let ((beg (map-nested-elt source-block '(:block :start)))
                   (message (error-message-string err)))
               (agent-shell-mermaid--clear-overlays
                beg (map-nested-elt source-block '(:block :end)))
               (agent-shell-mermaid--error-overlay beg message)
               (agent-shell-mermaid--log "failed: %s" message)))))))
    (when watermarks (list (cons :watermark (apply #'min watermarks))))))

(defun agent-shell-mermaid-render-context (context)
  "Render Mermaid fenced blocks described by Markdown CONTEXT."
  (agent-shell-mermaid-preview-blocks (map-elt context :source-blocks)))

(defun agent-shell-mermaid-refresh-buffer ()
  "Refresh all Mermaid previews in the current buffer."
  (interactive)
  (when (text-property-not-all (point-min) (point-max)
                               'agent-shell-mermaid-preview nil)
    (let ((inhibit-read-only t))
      (save-excursion
        (save-restriction
          (widen)
          (agent-shell-mermaid-preview-blocks
           (map-elt (agent-shell-markdown-context) :source-blocks) t))))))

(defun agent-shell-mermaid--refresh-buffers-after-theme (&rest _)
  "Refresh Mermaid previews in live buffers after a theme change."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (text-property-not-all (point-min) (point-max)
                                     'agent-shell-mermaid-preview nil)
          (condition-case err
              (agent-shell-mermaid-refresh-buffer)
            (error
             (agent-shell-mermaid--log "theme refresh failed in %s: %s"
                                        (buffer-name)
                                        (error-message-string err)))))))))

(add-hook 'agent-shell-markdown-render-functions
          #'agent-shell-mermaid-render-context)
(add-hook 'enable-theme-functions
          #'agent-shell-mermaid--refresh-buffers-after-theme)

(provide 'agent-shell-mermaid)

;;; agent-shell-mermaid.el ends here
