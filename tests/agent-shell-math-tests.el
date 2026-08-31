;;; agent-shell-math-tests.el --- Tests for agent-shell math renderer -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-math)

;;; Code:

(defun agent-shell-math-tests--fake-preview (beg end)
  "Mark BEG..END as rendered without requiring a GUI frame."
  (let ((source (buffer-substring-no-properties beg end)))
    (add-text-properties
     beg end
     (list 'agent-shell-math-preview t
           'agent-shell-markdown-frozen t
           'agent-shell-markdown-source source))))

(defun agent-shell-math-tests--overlays (kind)
  "Return math renderer overlays of KIND in the current buffer."
  (seq-filter (lambda (overlay)
                (eq (overlay-get overlay 'agent-shell-math-overlay) kind))
              (overlays-in (point-min) (point-max))))

(defmacro agent-shell-math-tests--with-fake-image-render (&rest body)
  "Run BODY with image support and LaTeX compilation stubbed."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'create-image)
               (lambda (&rest _) '(image :type svg :data "fake")))
              ((symbol-function 'agent-shell-math-render-latex-to-file)
               (lambda (&rest _) "/tmp/agent-shell-math-test.svg")))
      ,@body))

(ert-deftest agent-shell-math-detects-complete-fenced-latex-block-test ()
  "A complete latex fence is exposed through the Markdown render context."
  (with-temp-buffer
    (insert (concat "```latex\n3x + 2 < 11\n```"))
    (let ((block (car (map-elt (agent-shell-markdown-context) :source-blocks))))
      (should (equal "latex" (map-elt block :language)))
      (should (equal "3x + 2 < 11" (map-elt block :body)))
      (should (map-elt block :complete)))))

(ert-deftest agent-shell-math-renders-complete-fenced-latex-block-test ()
  "A complete latex fence is claimed before normal code-block styling."
  (with-temp-buffer
    (insert (concat "```latex\n3x + 2 < 11\n```\n"))
    (let ((calls 0))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'create-image)
                 (lambda (&rest _) '(image :type svg :data "fake")))
                ((symbol-function 'agent-shell-math-render-latex-to-file)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   "/tmp/agent-shell-math-test.svg")))
        (agent-shell-markdown-replace-markup :complete t))
      (should (= calls 1)))
    (should (get-text-property (point-min) 'agent-shell-math-preview))
    (should (get-text-property (point-min) 'display))
    (should (get-text-property (point-min) 'agent-shell-markdown-frozen))
    (should (equal "3x + 2 < 11"
                   (get-text-property (point-min) 'agent-shell-math-source)))
    ;; The raw Markdown remains in the buffer for copying, while its display
    ;; properties show the native preview and prevent the built-in code pass
    ;; from deleting or re-styling it.
    (should (string-match-p "```latex"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

(ert-deftest agent-shell-math-streaming-fenced-latex-block-test ()
  "An open latex fence waits for its closing fence, then renders."
  (with-temp-buffer
    (insert "before\n")
    (let ((block-beg (point)))
      (insert (concat "```latex\n3x + 2 < 11"))
    (let ((calls 0))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'create-image)
                 (lambda (&rest _) '(image :type svg :data "fake")))
                ((symbol-function 'agent-shell-math-render-latex-to-file)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   "/tmp/agent-shell-math-test.svg")))
        (agent-shell-markdown-replace-markup))
      (should (= calls 0)))
    (let ((pending (agent-shell-math-tests--overlays 'pending)))
      (should (= 1 (length pending)))
      (should (string-match-p "closing fence"
                              (overlay-get (car pending) 'display))))
    (goto-char (point-max))
    (insert (concat "\n```\n"))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'create-image)
               (lambda (&rest _) '(image :type svg :data "fake")))
              ((symbol-function 'agent-shell-math-render-latex-to-file)
               (lambda (&rest _) "/tmp/agent-shell-math-test.svg")))
      (agent-shell-markdown-replace-markup :complete t))
    (should-not (agent-shell-math-tests--overlays 'pending))
      (should (get-text-property block-beg 'agent-shell-math-preview)))))

(ert-deftest agent-shell-math-invalid-fenced-latex-falls-back-with-error-test ()
  "A compiler failure leaves source intact and reports an in-buffer error."
  (with-temp-buffer
    (insert (concat "```latex\n\\\\thiscommanddoesnotexist\n```\n"))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&optional _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'agent-shell-math-render-latex-to-file)
               (lambda (&rest _) (error "LaTeX compilation failed (1)"))))
      (agent-shell-markdown-replace-markup :complete t))
    (let ((errors (agent-shell-math-tests--overlays 'error)))
      (should (= 1 (length errors)))
      (should (string-match-p "LaTeX compilation failed"
                              (overlay-get (car errors) 'before-string))))
    (should (string-match-p "thiscommanddoesnotexist"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))
    (should-not (get-text-property (point-min) 'agent-shell-math-preview))))

(ert-deftest agent-shell-math-normal-code-block-regression-test ()
  "Ordinary fenced code remains a normal Markdown code block."
  (with-temp-buffer
    (insert (concat "```python\nprint('hello')\n```\n"))
    (agent-shell-math-tests--with-fake-image-render
      (agent-shell-markdown-replace-markup :complete t))
    (should-not (text-property-not-all (point-min) (point-max)
                                       'agent-shell-math-preview nil))
    (should (string-match-p "python .*⧉"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))
    (should (string-match-p "print('hello')"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

(ert-deftest agent-shell-math-render-markdown-accepts-agent-shell-keywords-test ()
  "The Doom/local math renderer accepts the agent-shell renderer contract."
  (with-temp-buffer
    (insert "Aquí estamos, en pie y con `\\(x\\)` literal.\n")
    (should
     (condition-case nil
         (progn
           (agent-shell-math-render-markdown
            :render-images nil
            :highlight-blocks nil
            :image-cache-directory temporary-file-directory
            :future-keyword t)
           t)
       (error nil)))
    (should (string-match-p "Aquí estamos"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

(ert-deftest agent-shell-math-renders-before-markdown-escapes-test ()
  "Math is claimed before Markdown escape decoding removes delimiters."
  (with-temp-buffer
    (let ((formula "\\[\\frac{x}{3}+4=\\frac{x-3}{6}+10\\]") rendered)
      (insert formula)
      (cl-letf (((symbol-function 'agent-shell-math--preview-source)
                 (lambda (beg end)
                   (push (buffer-substring-no-properties beg end) rendered)
                   (agent-shell-math-tests--fake-preview beg end))))
        (agent-shell-math-render-markdown))
      (should (equal rendered (list formula)))
      (should (eq t (get-text-property (point-min) 'agent-shell-markdown-frozen)))
      (should (equal formula (get-text-property (point-min) 'agent-shell-markdown-source))))))

(ert-deftest agent-shell-math-holds-open-streaming-delimiter-test ()
  "Incomplete display math stays intact across multiple streaming chunks."
  (with-temp-buffer
    (let ((agent-shell-markdown-render-functions
           '(agent-shell-math-render-context))
          (formula "\\[\n\\frac{x}{3}\\]"))
      (insert "intro\n\\[")
      (let ((open (- (point-max) 2)))
        (agent-shell-markdown-replace-markup)
        (should (= open (agent-shell-markdown--watermark-start)))
        (should (eq t (get-text-property open 'agent-shell-math-pending)))
        (should (equal "\\[" (buffer-substring-no-properties open (point-max))))
        (goto-char (point-max))
        (insert "\n\\frac{x")
        (should-not (get-text-property (1- (point-max))
                                       'agent-shell-math-pending))
        (agent-shell-markdown-replace-markup)
        (should (= open (agent-shell-markdown--watermark-start)))
        (should (equal "\\[\n\\frac{x"
                       (buffer-substring-no-properties open (point-max))))
        (goto-char (point-max))
        (insert "}{3}\\]")
        (cl-letf (((symbol-function 'agent-shell-math--preview-source)
                   #'agent-shell-math-tests--fake-preview))
          (agent-shell-markdown-replace-markup :complete t))
        (should-not (get-text-property open 'agent-shell-math-pending))
        (should (eq t (get-text-property open
                                         'agent-shell-markdown-frozen)))
        (should (equal formula
                       (get-text-property open
                                          'agent-shell-markdown-source)))))))

(ert-deftest agent-shell-math-registers-render-context-test ()
  "Loading the math extension registers its pre-Markdown renderer."
  (should (memq #'agent-shell-math-render-context
                agent-shell-markdown-render-functions)))

(provide 'agent-shell-math-tests)

;;; agent-shell-math-tests.el ends here
