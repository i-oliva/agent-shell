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

(provide 'agent-shell-math-tests)

;;; agent-shell-math-tests.el ends here
