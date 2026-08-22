;;; agent-shell-mermaid-tests.el --- Tests for Mermaid rendering -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-mermaid)

;;; Code:

(defmacro agent-shell-mermaid-tests--with-fake-render (&rest body)
  "Run BODY with image support and Mermaid CLI rendering stubbed."
  (declare (indent 0) (debug t))
  \`(cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'create-image)
               (lambda (&rest _) '(image :type svg :data "fake")))
              ((symbol-function 'agent-shell-mermaid-render-to-file)
               (lambda (&rest _) "/tmp/agent-shell-mermaid-test.svg")))
     ,@body))

(defun agent-shell-mermaid-tests--overlays (kind)
  "Return Mermaid overlays of KIND in the current buffer."
  (seq-filter (lambda (overlay)
                (eq (overlay-get overlay 'agent-shell-mermaid-overlay) kind))
              (overlays-in (point-min) (point-max))))

(ert-deftest agent-shell-mermaid-detects-complete-blocks-test ()
  "Markdown context detects Mermaid language, body, and completion."
  (with-temp-buffer
    (insert "\`\`\`mermaid\nflowchart LR\n  A --> B\n\`\`\`\n")
    (let ((block (car (map-elt (agent-shell-markdown-context) :source-blocks))))
      (should (equal "mermaid" (map-elt block :language)))
      (should (equal "flowchart LR\n  A --> B" (map-elt block :body)))
      (should (map-elt block :complete)))))

(ert-deftest agent-shell-mermaid-streaming-placeholder-test ()
  "An open Mermaid fence shows a placeholder and holds the watermark."
  (with-temp-buffer
    (insert "before\n\`\`\`mermaid\nflowchart LR\n  A -->")
    (agent-shell-markdown-replace-markup)
    (let ((pending (agent-shell-mermaid-tests--overlays 'pending)))
      (should (= 1 (length pending)))
      (should (string-match-p "closing fence"
                              (overlay-get (car pending) 'display)))
      (should (string-suffix-p "A -->" (buffer-string)))
      (should (< (agent-shell-markdown--watermark-start) (point-max))))))

(ert-deftest agent-shell-mermaid-renders-after-closing-fence-test ()
  "A streamed Mermaid block renders after its closing fence arrives."
  (with-temp-buffer
    (insert "\`\`\`mermaid\nflowchart TD\n  A --> B")
    (agent-shell-markdown-replace-markup)
    (should (agent-shell-mermaid-tests--overlays 'pending))
    (goto-char (point-max))
    (insert "\n\`\`\`\n")
    (agent-shell-mermaid-tests--with-fake-render
      (agent-shell-markdown-replace-markup :complete t))
    (should-not (agent-shell-mermaid-tests--overlays 'pending))
    (should (get-text-property (point-min) 'agent-shell-mermaid-preview))
    (should (equal "flowchart TD\n  A --> B"
                   (get-text-property (point-min) 'agent-shell-mermaid-source)))))

(ert-deftest agent-shell-mermaid-renders-multiple-diagrams-test ()
  "Several Mermaid blocks render independently with unique ids."
  (with-temp-buffer
    (insert "\`\`\`mermaid\nflowchart LR\nA-->B\n\`\`\`\ntext\n\`\`\`mermaid\nsequenceDiagram\nA->>B: hola\n\`\`\`\n")
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

(ert-deftest agent-shell-mermaid-rerenders-for-theme-change-test ()
  "A theme change replaces the cached image without changing source."
  (with-temp-buffer
    (insert "\`\`\`mermaid\nflowchart LR\nA-->B\n\`\`\`\n")
    (let ((theme "dark") calls)
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

(ert-deftest agent-shell-mermaid-invalid-source-fallback-test ()
  "Invalid Mermaid stays visible as code and carries a useful error."
  (with-temp-buffer
    (insert "\`\`\`mermaid\nthis is not valid mermaid !!!\n\`\`\`\n")
    (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'agent-shell-mermaid-render-to-file)
               (lambda (&rest _) (error "Parse error on line 1"))))
      (agent-shell-markdown-replace-markup :complete t))
    (let ((errors (agent-shell-mermaid-tests--overlays 'error)))
      (should (= 1 (length errors)))
      (should (string-match-p "Parse error on line 1"
                              (overlay-get (car errors) 'before-string))))
    (should (string-match-p "this is not valid mermaid"
                            (buffer-substring-no-properties (point-min) (point-max))))
    (should-not (get-text-property (point-min) 'agent-shell-mermaid-preview))))

(ert-deftest agent-shell-mermaid-unicode-long-labels-test ()
  "Unicode and long labels reach Mermaid unchanged."
  (with-temp-buffer
    (let ((source "flowchart LR\n  A[Conexión → análisis con etiqueta muy larga: áéíóú 漢字] --> B[Terminado ✓]")
          rendered)
      (insert (format "\`\`\`mermaid\n%s\n\`\`\`\n" source))
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
  "Non-Mermaid fenced blocks retain normal Markdown rendering."
  (with-temp-buffer
    (insert "\`\`\`python\nprint(\"hello\")\n\`\`\`\n")
    (agent-shell-mermaid-tests--with-fake-render
      (agent-shell-markdown-replace-markup :complete t))
    (should-not (text-property-not-all (point-min) (point-max)
                                       'agent-shell-mermaid-preview nil))
    (should (string-match-p "python .*⧉"
                            (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-match-p "print(\"hello\")"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest agent-shell-mermaid-generated-svg-uses-native-text-test ()
  "Real Mermaid output avoids foreignObject labels unsupported by Emacs SVG."
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
            (should-not (search-forward "<foreignObject" nil t))
            (should (search-forward "id=\"agent-shell-mermaid-test-svg\"" nil t))))
      (delete-directory agent-shell-mermaid-cache-directory t))))

(provide 'agent-shell-mermaid-tests)

;;; agent-shell-mermaid-tests.el ends here
