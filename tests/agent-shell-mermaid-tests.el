;;; agent-shell-mermaid-tests.el --- Tests for Mermaid rendering -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-mermaid)

;;; Code:

(defmacro agent-shell-mermaid-tests--with-fake-render (&rest body)
  "Run BODY with image support and Mermaid CLI rendering stubbed."
  (declare (indent 0) (debug t))
  `(let ((agent-shell-mermaid-render-asynchronously nil))
     (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'create-image)
               (lambda (&rest _) '(image :type svg :data "fake")))
              ((symbol-function 'agent-shell-mermaid-render-to-file)
               (lambda (&rest _) "/tmp/agent-shell-mermaid-test.svg")))
       ,@body)))

(defun agent-shell-mermaid-tests--overlays (kind)
  "Return Mermaid overlays of KIND in the current buffer."
  (seq-filter (lambda (overlay)
                (eq (overlay-get overlay 'agent-shell-mermaid-overlay) kind))
              (overlays-in (point-min) (point-max))))

(ert-deftest agent-shell-mermaid-detects-complete-blocks-test ()
  (with-temp-buffer
    (insert "```mermaid\nflowchart LR\n  A --> B\n```\n")
    (let ((block (car (map-elt (agent-shell-markdown-context) :source-blocks))))
      (should (equal "mermaid" (map-elt block :language)))
      (should (equal "flowchart LR\n  A --> B" (map-elt block :body)))
      (should (map-elt block :complete)))))

(ert-deftest agent-shell-mermaid-streaming-placeholder-test ()
  (with-temp-buffer
    (insert "before\n```mermaid\nflowchart LR\n  A -->")
    (agent-shell-markdown-replace-markup)
    (let ((pending (agent-shell-mermaid-tests--overlays 'pending)))
      (should (= 1 (length pending)))
      (should (string-match-p "closing fence"
                              (overlay-get (car pending) 'display)))
      (should (string-suffix-p "A -->" (buffer-string)))
      (should (< (agent-shell-markdown--watermark-start) (point-max))))))

(ert-deftest agent-shell-mermaid-renders-after-closing-fence-test ()
  (with-temp-buffer
    (insert "```mermaid\nflowchart TD\n  A --> B")
    (agent-shell-markdown-replace-markup)
    (should (agent-shell-mermaid-tests--overlays 'pending))
    (goto-char (point-max))
    (insert "\n```\n")
    (agent-shell-mermaid-tests--with-fake-render
      (agent-shell-markdown-replace-markup :complete t))
    (should-not (agent-shell-mermaid-tests--overlays 'pending))
    (should (get-text-property (point-min) 'agent-shell-mermaid-preview))
    (should (equal "flowchart TD\n  A --> B"
                   (get-text-property (point-min) 'agent-shell-mermaid-source)))))

(ert-deftest agent-shell-mermaid-renders-multiple-diagrams-test ()
  (with-temp-buffer
    (insert "```mermaid\nflowchart LR\nA-->B\n```\ntext\n```mermaid\nsequenceDiagram\nA->>B: hola\n```\n")
    (agent-shell-mermaid-tests--with-fake-render
      (agent-shell-markdown-replace-markup :complete t))
    (let ((pos (point-min)) ids)
      (while (< pos (point-max))
        (when-let* ((id (get-text-property pos 'agent-shell-mermaid-id)))
          (push id ids))
        (setq pos (next-single-property-change
                   pos 'agent-shell-mermaid-id nil (point-max))))
      (should (= 2 (length ids)))
      (should-not (equal (car ids) (cadr ids))))))

(ert-deftest agent-shell-mermaid-detects-effective-face-theme-test ()
  (let ((agent-shell-mermaid-theme 'auto)
        rgb)
    (cl-letf (((symbol-function 'agent-shell-mermaid--display-frame)
               (lambda () 'test-frame))
              ((symbol-function 'face-background)
               (lambda (&rest _) "test-background"))
              ((symbol-function 'color-values)
               (lambda (&rest _) rgb)))
      (setq rgb '(4096 4096 4096))
      (should (equal "dark" (agent-shell-mermaid--theme)))
      (setq rgb '(61440 61440 61440))
      (should (equal "default" (agent-shell-mermaid--theme))))))

(ert-deftest agent-shell-mermaid-rerenders-for-theme-change-test ()
  (with-temp-buffer
    (insert "```mermaid\nflowchart LR\nA-->B\n```\n")
    (let ((agent-shell-mermaid-render-asynchronously nil)
          (theme "dark") calls)
      (cl-letf (((symbol-function 'agent-shell-mermaid--theme) (lambda () theme))
                ((symbol-function 'display-images-p) (lambda (&optional _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'create-image)
                 (lambda (file &rest _) (list 'image :file file)))
                ((symbol-function 'agent-shell-mermaid-render-to-file)
                 (lambda (_source _id selected-theme)
                   (push selected-theme calls)
                   (format "/tmp/%s.svg" selected-theme))))
        (agent-shell-markdown-replace-markup :complete t)
        (setq theme "default")
        (agent-shell-mermaid-refresh-buffer)
        (should (equal '("default" "dark") calls))
        (should (equal "default"
                       (get-text-property (point-min) 'agent-shell-mermaid-theme)))))))

(ert-deftest agent-shell-mermaid-refresh-uses-stored-preview-test ()
  (with-temp-buffer
    (insert "```mermaid
flowchart LR
A-->B
```
")
    (let ((agent-shell-mermaid-render-asynchronously nil)
          (end (point-max))
          (theme "dark")
          calls)
      (cl-letf (((symbol-function 'agent-shell-mermaid--theme)
                 (lambda () theme))
                ((symbol-function 'display-images-p) (lambda (&optional _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'create-image)
                 (lambda (&rest _) '(image :type svg :data "fake")))
                ((symbol-function 'agent-shell-mermaid-render-to-file)
                 (lambda (_source _id selected-theme)
                   (push selected-theme calls)
                   "/tmp/agent-shell-mermaid-test.svg"))
                ((symbol-function 'agent-shell-markdown-context)
                 (lambda (&rest _) '((:source-blocks . nil)))))
        (agent-shell-mermaid-preview-blocks
         (list
          (list (cons :language "mermaid")
                (cons :body "flowchart LR
A-->B")
                (cons :complete t)
                (cons :block
                      (list (cons :start 1) (cons :end end))))))
        (setq theme "default")
        (agent-shell-mermaid-refresh-buffer)
        (should (equal '("default" "dark") calls))
        (should (equal "default"
                       (get-text-property
                        (point-min) 'agent-shell-mermaid-theme)))))))

(ert-deftest agent-shell-mermaid-uncached-render-is-asynchronous-test ()
  (with-temp-buffer
    (insert "```mermaid\nflowchart LR\nA-->B\n```\n")
    (let ((agent-shell-mermaid-render-asynchronously t)
          callback)
      (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'agent-shell-mermaid-render-to-file-async)
                 (lambda (_source _id _theme function)
                   (setq callback function)
                   nil)))
        (agent-shell-markdown-replace-markup :complete t)
        (should callback)
        (should (agent-shell-mermaid-tests--overlays 'rendering))
        (should-not (get-text-property (point-min)
                                       'agent-shell-mermaid-preview))
        (funcall callback "/tmp/agent-shell-mermaid-test.svg" nil)
        (should-not (agent-shell-mermaid-tests--overlays 'rendering))
        (should (get-text-property (point-min)
                                   'agent-shell-mermaid-preview))))))

(ert-deftest agent-shell-mermaid-cached-async-result-has-no-stale-placeholder-test ()
  (with-temp-buffer
    (insert "\x60\x60\x60mermaid\nflowchart LR\nA-->B\n\x60\x60\x60\n")
    (let ((agent-shell-mermaid-render-asynchronously t))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'create-image)
                 (lambda (&rest _) '(image :type svg :data "fake")))
                ((symbol-function 'agent-shell-mermaid-render-to-file-async)
                 (lambda (_source _id _theme callback)
                   (funcall callback "/tmp/agent-shell-mermaid-test.svg" nil)
                   nil)))
        (agent-shell-mermaid-preview-blocks
         (map-elt (agent-shell-markdown-context) :source-blocks))
        (should-not (agent-shell-mermaid-tests--overlays 'rendering))
        (should (get-text-property (point-min)
                                   'agent-shell-mermaid-preview))))))

(ert-deftest agent-shell-mermaid-does-not-restart-matching-render-test ()
  (with-temp-buffer
    (insert "```mermaid\nflowchart LR\nA-->B\n```\n")
    (let ((agent-shell-mermaid-render-asynchronously t)
          callback
          (calls 0))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'create-image)
                 (lambda (&rest _) '(image :type svg :data "fake")))
                ((symbol-function 'agent-shell-mermaid-render-to-file-async)
                 (lambda (_source _id _theme function)
                   (setq callback function
                         calls (1+ calls))
                   nil)))
        (agent-shell-markdown-replace-markup :complete t)
        (agent-shell-markdown-replace-markup :complete t)
        (should (= calls 1))
        (should (agent-shell-mermaid-tests--overlays 'rendering))
        (funcall callback "/tmp/agent-shell-mermaid-test.svg" nil)
        (should (get-text-property (point-min)
                                   'agent-shell-mermaid-preview))))))

(ert-deftest agent-shell-mermaid-ignores-obsolete-async-result-test ()
  (with-temp-buffer
    (insert "```mermaid\nflowchart LR\nA-->B\n```\n")
    (let ((agent-shell-mermaid-render-asynchronously t)
          (end (point-max))
          callbacks)
      (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'create-image)
                 (lambda (&rest _) '(image :type svg :data "fake")))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'delete-process) (lambda (_) nil))
                ((symbol-function 'agent-shell-mermaid-render-to-file-async)
                 (lambda (_source _id _theme callback)
                   (setq callbacks (append callbacks (list callback)))
                   'fake-process)))
        (agent-shell-mermaid--preview-block
         `((:language . "mermaid")
           (:body . "flowchart LR\nA-->B")
           (:complete . t)
           (:block . ((:start . 1) (:end . ,end)))))
        (agent-shell-mermaid--preview-block
         `((:language . "mermaid")
           (:body . "flowchart LR\nA-->C")
           (:complete . t)
           (:block . ((:start . 1) (:end . ,end)))))
        (should (= (length callbacks) 2))
        (funcall (car callbacks) "/tmp/obsolete.svg" nil)
        (should-not (get-text-property (point-min)
                                       'agent-shell-mermaid-preview))
        (funcall (cadr callbacks) "/tmp/current.svg" nil)
        (should (equal "flowchart LR\nA-->C"
                       (get-text-property
                        (point-min) 'agent-shell-mermaid-source)))))))

(ert-deftest agent-shell-mermaid-invalid-source-fallback-test ()
  (with-temp-buffer
    (insert "```mermaid\nthis is not valid mermaid !!!\n```\n")
    (let ((agent-shell-mermaid-render-asynchronously nil))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'agent-shell-mermaid-render-to-file)
               (lambda (&rest _) (error "Parse error on line 1"))))
        (agent-shell-markdown-replace-markup :complete t)))
    (let ((errors (agent-shell-mermaid-tests--overlays 'error)))
      (should (= 1 (length errors)))
      (should (string-match-p "Parse error on line 1"
                              (overlay-get (car errors) 'before-string))))
    (should (string-match-p "this is not valid mermaid"
                            (buffer-substring-no-properties (point-min) (point-max))))
    (should-not (get-text-property (point-min) 'agent-shell-mermaid-preview))))

(ert-deftest agent-shell-mermaid-unicode-long-labels-test ()
  (with-temp-buffer
    (let ((source "flowchart LR\n  A[Conexión → análisis con etiqueta muy larga: áéíóú 漢字] --> B[Terminado ✓]")
          rendered)
      (insert (format "```mermaid\n%s\n```\n" source))
      (agent-shell-mermaid-tests--with-fake-render
        (cl-letf (((symbol-function 'agent-shell-mermaid-render-to-file)
                   (lambda (body &rest _)
                     (setq rendered body)
                     "/tmp/agent-shell-mermaid-test.svg")))
          (agent-shell-markdown-replace-markup :complete t)))
      (should (equal source rendered))
      (should (equal source (get-text-property
                             (point-min) 'agent-shell-mermaid-source))))))

(ert-deftest agent-shell-mermaid-normal-code-block-regression-test ()
  (with-temp-buffer
    (insert "```python\nprint(\"hello\")\n```\n")
    (agent-shell-mermaid-tests--with-fake-render
      (agent-shell-markdown-replace-markup :complete t))
    (should-not (text-property-not-all (point-min) (point-max)
                                       'agent-shell-mermaid-preview nil))
    (should (string-match-p "python .*⧉"
                            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match-p "print(\"hello\")"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest agent-shell-mermaid-generated-svg-uses-native-text-test ()
  (skip-unless (and (agent-shell-mermaid--mmdc-command)
                    (agent-shell-mermaid--browser-executable)))
  (let ((agent-shell-mermaid-cache-directory
         (make-temp-file "agent-shell-mermaid-tests-" t)))
    (unwind-protect
        (let ((file (agent-shell-mermaid-render-to-file
                     "flowchart LR\n  A[Unicode → etiqueta larga] --> B[Fin ✓]"
                     "agent-shell-mermaid-test-svg" "dark")))
          (with-temp-buffer
            (insert-file-contents-literally file)
            (should (search-forward "<text" nil t))
            (goto-char (point-min))
            (should-not (search-forward "<foreignObject" nil t))
            (goto-char (point-min))
            (should (search-forward "id=\"agent-shell-mermaid-test-svg\"" nil t))))
      (delete-directory agent-shell-mermaid-cache-directory t))))

(provide 'agent-shell-mermaid-tests)

;;; agent-shell-mermaid-tests.el ends here
