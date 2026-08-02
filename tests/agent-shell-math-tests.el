;;; agent-shell-math-tests.el --- Tests for agent-shell math renderer -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-math)

;;; Code:

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

(provide 'agent-shell-math-tests)

;;; agent-shell-math-tests.el ends here
