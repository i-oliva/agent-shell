;;; agent-shell-modeline-tests.el --- Mode line tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

;;; Code:

(defmacro agent-shell-modeline-tests--with-shell (&rest body)
  "Run BODY with a real shell mode and synthetic session settings."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((agent-shell-mode-hook nil)) (agent-shell-mode))
     (setq-local agent-shell-header-style 'none)
     (setq-local agent-shell-show-context-usage-indicator nil)
     (setq-local agent-shell-show-busy-indicator nil)
     (setq-local agent-shell--state
                 (agent-shell--make-state :buffer (current-buffer)))
     (map-put! agent-shell--state :config-options
               (mapcar (lambda (category)
                         (copy-tree
                          `((:id . ,category) (:category . ,category)
                            (:type . "select") (:current-value . "a")
                            (:options . (((:value . "a") (:name . "Alpha"))
                                         ((:value . "b") (:name . "Beta")))))))
                       '("model" "mode" "thought_level")))
     (setq-local mode-line-misc-info nil)
     (agent-shell--setup-modeline)
     ,@body))

(defun agent-shell-modeline-tests--render ()
  "Evaluate the shell's registered mode line segment."
  (eval (cadar mode-line-misc-info) t))

(ert-deftest agent-shell-modeline-redraw-does-not-build-menus-test ()
  "Redrawing live status must not enumerate the settings menus."
  (agent-shell-modeline-tests--with-shell
    (cl-letf (((symbol-function 'agent-shell--mode-line-model-menu)
               (lambda () (ert-fail "Model menu built during redisplay")))
              ((symbol-function 'agent-shell--mode-line-mode-menu)
               (lambda () (ert-fail "Mode menu built during redisplay")))
              ((symbol-function 'agent-shell--mode-line-thought-level-menu)
               (lambda () (ert-fail "Thought menu built during redisplay"))))
      (dotimes (_ 20)
        (should (equal (substring-no-properties
                        (agent-shell-modeline-tests--render))
                       " Alpha ➤ Alpha ➤ Alpha"))))))

(ert-deftest agent-shell-modeline-menus-read-current-settings-in-origin-test ()
  "Each menu opens with fresh choices in its shell, even from another buffer."
  (agent-shell-modeline-tests--with-shell
    (let ((shell (current-buffer))
          (status (agent-shell-modeline-tests--render)))
      (dolist (setting '((agent-shell-model "model" model-c)
                         (agent-shell-session-mode "mode" mode-c)
                         (agent-shell-thought-level "thought_level" thought-level-c)))
        (let* ((option (agent-shell--config-option-by-category
                        agent-shell--state (nth 1 setting)))
               (start (text-property-any 0 (length status) 'face (car setting) status))
               (map (get-text-property start 'local-map status)))
          (should (eq (get-text-property start 'mouse-face status) 'mode-line-highlight))
          (map-put! option :options
                    '(((:value . "c") (:name . "Gamma"))))
          (map-put! option :current-value "c")
          (with-temp-buffer
            (let* ((menu (lookup-key map [mode-line mouse-1]))
                   (item (assq (nth 2 setting) (cdr menu)))
                   selected)
              (should (keymapp menu))
              (should (equal (nth 2 item) "Gamma"))
              (should (equal (plist-get (nthcdr 4 item) :button) '(:toggle . t)))
              (cl-letf (((symbol-function 'agent-shell--config-option-set-model-id)
                         (lambda (&rest args) (setq selected (list (current-buffer) args))))
                        ((symbol-function 'agent-shell--config-option-set-mode-id)
                         (lambda (&rest args) (setq selected (list (current-buffer) args))))
                        ((symbol-function 'agent-shell--config-option-set-thought-level-id)
                         (lambda (&rest args) (setq selected (list (current-buffer) args)))))
                (call-interactively (nth 3 item)))
              (should (eq (car selected) shell))
              (should (equal (cadr (cadr selected)) "c")))))))))

(ert-deftest agent-shell-modeline-dead-shell-menu-is-inert-test ()
  "An old status string must not open a menu or tooltip for a killed shell."
  (let (map help)
    (agent-shell-modeline-tests--with-shell
      (let ((status (agent-shell-modeline-tests--render)))
        (setq map (get-text-property 1 'local-map status)
              help (get-text-property 1 'help-echo status))))
    (should-not (lookup-key map [mode-line mouse-1]))
    (should-not (funcall help nil nil 1))))

(ert-deftest agent-shell-modeline-model-label-does-not-copy-all-choices-test ()
  "Showing the selected model must not convert the entire model catalogue."
  (agent-shell-modeline-tests--with-shell
    (cl-letf (((symbol-function 'agent-shell--config-option-as-models)
               (lambda (_) (ert-fail "Model catalogue copied during redisplay"))))
      (should (equal (substring-no-properties
                      (agent-shell-modeline-tests--render))
                     " Alpha ➤ Alpha ➤ Alpha")))))

(ert-deftest agent-shell-modeline-shortcuts-are-resolved-only-on-hover-test ()
  "Redisplay does not search keymaps; hovering resolves the current shortcut."
  (agent-shell-modeline-tests--with-shell
    (let ((lookups 0))
      (cl-letf (((symbol-function 'where-is-internal)
                 (lambda (&rest _)
                   (setq lookups (1+ lookups))
                   [f12])))
        (let ((status (agent-shell-modeline-tests--render)))
          (should (= lookups 0))
          (let ((help (get-text-property 1 'help-echo status)))
            (should (functionp help))
            (should (equal (substring-no-properties (funcall help nil nil 1))
                           "Open LLM model menu <f12>")))
          (should (= lookups 1)))))))

(ert-deftest agent-shell-modeline-tooltips-use-current-origin-keybindings-test ()
  "Each tooltip uses its shell's current binding, even from another buffer."
  (agent-shell-modeline-tests--with-shell
    (setq-local agent-shell-mode-map (make-sparse-keymap))
    (let ((map agent-shell-mode-map)
          (status (agent-shell-modeline-tests--render)))
      (dolist (setting '((agent-shell-model agent-shell-set-session-model "LLM model")
                         (agent-shell-session-mode agent-shell-set-session-mode "session mode")
                         (agent-shell-thought-level agent-shell-set-session-thought-level "thought level")))
        (let* ((start (text-property-any 0 (length status) 'face (car setting) status))
               (help (get-text-property start 'help-echo status)))
          (define-key map [f12] (nth 1 setting))
          (with-temp-buffer
            (setq-local agent-shell-mode-map (make-sparse-keymap))
            (define-key agent-shell-mode-map [f8] (nth 1 setting))
            (should (equal (substring-no-properties (funcall help nil status start))
                           (concat "Open " (nth 2 setting) " menu <f12>")))
            (define-key map [f12] nil)
            (define-key map [f11] (nth 1 setting))
            (should (equal (substring-no-properties (funcall help nil status start))
                           (concat "Open " (nth 2 setting) " menu <f11>")))
            (define-key map [f11] nil)
            (should (equal (funcall help nil status start)
                           (concat "Open " (nth 2 setting) " menu ")))))))))

(ert-deftest agent-shell-modeline-reopening-menu-reads-new-options-test ()
  "Reusing the same status string never reuses stale menu choices."
  (agent-shell-modeline-tests--with-shell
    (let* ((status (agent-shell-modeline-tests--render))
           (map (get-text-property 1 'local-map status))
           (option (agent-shell--config-option-by-category agent-shell--state "model"))
           (first (lookup-key map [mode-line mouse-1])))
      (should (equal (nth 2 (assq 'model-a (cdr first))) "Alpha"))
      (map-put! option :options '(((:value . "c") (:name . "Gamma"))))
      (map-put! option :current-value "c")
      (let ((second (lookup-key map [mode-line mouse-1])))
        (should-not (eq first second))
        (should-not (assq 'model-a (cdr second)))
        (should (equal (nth 2 (assq 'model-c (cdr second))) "Gamma"))))))

(ert-deftest agent-shell-modeline-rendering-preserves-conversation-test ()
  "Redraw, hover and menu lookup leave the conversation and timers untouched."
  (agent-shell-modeline-tests--with-shell
    (buffer-enable-undo)
    (insert (propertize "Unsent draft" 'face 'bold))
    (goto-char 4)
    (let ((text (buffer-string))
          (state (copy-tree agent-shell--state))
          (position (point))
          (tick (buffer-modified-tick))
          (modified (buffer-modified-p))
          (undo buffer-undo-list)
          (timers (copy-sequence timer-list))
          (idle-timers (copy-sequence timer-idle-list))
          (gc-threshold gc-cons-threshold))
      (dotimes (_ 20)
        (let ((status (agent-shell-modeline-tests--render)))
          (lookup-key (get-text-property 1 'local-map status) [mode-line mouse-1])
          (funcall (get-text-property 1 'help-echo status) nil status 1)))
      (should (equal-including-properties text (buffer-string)))
      (should (equal state agent-shell--state))
      (should (= position (point)))
      (should (= tick (buffer-modified-tick)))
      (should (eq modified (buffer-modified-p)))
      (should (eq undo buffer-undo-list))
      (should (equal timers timer-list))
      (should (equal idle-timers timer-idle-list))
      (should (= gc-threshold gc-cons-threshold)))))

(ert-deftest agent-shell-modeline-model-label-preserves-fallbacks-test ()
  "Model labels prefer config choices, then retain the legacy name/ID fallback."
  (agent-shell-modeline-tests--with-shell
    (let ((session (map-elt agent-shell--state :session))
          (option (agent-shell--config-option-by-category agent-shell--state "model")))
      (map-put! session :model-id "legacy")
      (map-put! session :models '(((:model-id . "legacy") (:name . "Legacy"))))
      (should (string-prefix-p " Alpha ➤" (agent-shell-modeline-tests--render)))
      (map-put! option :current-value "b")
      (should (string-prefix-p " Beta ➤" (agent-shell-modeline-tests--render)))
      (map-put! option :current-value "unknown")
      (should (string-prefix-p " unknown ➤" (agent-shell-modeline-tests--render)))
      (map-put! option :current-value nil)
      (should (string-prefix-p " legacy ➤" (agent-shell-modeline-tests--render)))
      (map-put! agent-shell--state :config-options nil)
      (should (equal (substring-no-properties (agent-shell-modeline-tests--render))
                     " Legacy"))
      (map-put! session :model-id "unlisted")
      (should (equal (substring-no-properties (agent-shell-modeline-tests--render))
                     " unlisted"))
      (map-put! session :model-id nil)
      (should (equal (agent-shell-modeline-tests--render) "")))))

(provide 'agent-shell-modeline-tests)
;;; agent-shell-modeline-tests.el ends here
