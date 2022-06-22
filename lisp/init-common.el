;;; init-common.el --- Common library -*- lexical-binding: t -*-
;;; Commentary:
;; This is the PEW common library file.
;; It might be splitted into several files if it's needed in the future.
;; NOTE: This file should be loaded before any other packages.

;;; Code:
;;; Need to be evaluated at compile-time
(eval-and-compile
  (defmacro pew/set-custom (&rest customs)
    "Set CUSTOMS variables.
CUSTOMS is a list of the form:
  (VAR VALUE VAR VALUE ...)
This macro quotes VAR, constructs a list and passes it to `pew/set-custom*'."
    (declare (indent 0))
    (if (pew/oddp (length customs))
        (error "Incomplete variables and values"))
    (let ((customs_ customs)
          (args_ nil))
      (while customs_
        (push `'(,(pop customs_) ,(pop customs_)) args_))
      `(pew/set-custom* ,@(reverse args_))))

  (defun pew/set-custom* (&rest customs)
    "Set a list of CUSTOMS.
Each custom element is a list of the form:
  (VAR VALUE [COMMENT])
The VALUE will be evaluated before passing to `customize-set-variable'."
    (declare (indent 0))
    (dolist (custom_ customs)
      (customize-set-variable (pop custom_) (eval (pop custom_)) (pop custom_))))

  (defun pew/set-face (&rest faces)
    "Set FACES attributes.
FACES is a list of the form:
  ('(FACE ATTR VALUE ATTR VALUE ...) '(FACE ATTR VALUE ATTR VALUE ...) ...)
Each element will be passed to `set-face-attribute'.
Equivalent to:
  (set-face-attribute FACE nil ATTR VALUE ATTR VALUE ...)"
    (declare (indent 0))
    (dolist (attr_ faces)
      (apply 'set-face-attribute (pop attr_) nil attr_)))

  (defun pew/set-key (map &rest bindings)
    "Bind BINDINGS in MAP.
BINDINGS is a list of the form:
  (KEY DEF KEY DEF ...)
For DEF's definition see `define-key'.
The arguments will be collected in pairs and passed to `define-key'.
Equivalent to:
  (define-key MAP KEY DEF)"
    (declare (indent 1))
    (if (pew/oddp (length bindings))
        (error "Incomplete keys and definitions"))
    (let ((bindings_ bindings))
      (while bindings_
        (define-key map (pew/tokey (pop bindings_)) (pop bindings_)))))

  (defmacro pew/define-keymap (newmap &rest bindings)
    "Create a new map with name NEWMAP and bind key BINDINGS with it.
BINDINGS is a list of the form:
  (KEY DEF KEY DEF ...)
For DEF's definition see `define-key'.
The arguments will be collected in pairs and passed to `define-key'.
Equivalent to:
  (define-key NEWMAP KEY DEF)"
    (declare (indent 1))
    `(let ((map_ (make-sparse-keymap))
           (bindings_ (list ,@bindings)))
       (while bindings_
         (define-key map_ (pew/tokey (pop bindings_)) (pop bindings_)))
       (defvar ,newmap map_ "Keymap created by `pew/define-keymap'")))

  (defmacro pew/define-transient-command (cmd &rest bindings)
    "Create a transient map for a dummy command CMD and its key BINDINGS.
CMD is the name of the command.
This function will create two interactive commands CMD and CMD-repeat as well as
a keymap CMD-map.
Once CMD is invoked CMD-map will be temporarily activated.  The difference
between CMD and CMD-repeat is CMD only receive one followed key press while
CMD-repeat keeps receiving key press until an undefined key passed.
See `set-transient-map'.
Obsoleted `repeat-map' property method in Emacs 28 since it didn't work well for
some reason:
  (put cmd 'repeat-map map-symbol)
BINDINGS is a list of the form:
  (KEY DEF KEY DEF ...)
For DEF's definition see `define-key'."
    (declare (indent 1))
    (let ((map-symbol_ (intern (concat (symbol-name cmd) "-map")))
          (cmd-repeat-symbol_ (intern (concat (symbol-name cmd) "-repeat")))
          (cmd-doc-string_ "Command created by `pew/define-transient-command'"))
      `(progn
         ;; Create the map used in transient mode
         (pew/define-keymap ,map-symbol_ ,@bindings)
         ;; Create the commands
         (defun ,cmd ()
           ,cmd-doc-string_
           (interactive)
           (message "%s activated" ',cmd)
           (set-transient-map ,map-symbol_ nil))
         (defun ,cmd-repeat-symbol_ ()
           ,cmd-doc-string_
           (interactive)
           (message "%s activated" ',cmd-repeat-symbol_)
           (set-transient-map ,map-symbol_ t)))))

  (defun pew/set-property (&rest properties)
    "Set PROPERTIES for symbols.
Each element in PROPERTIES is of the form:
  (SYM PROP VAL)
The arguments will be passed to `put' one by one.
Equivalent to:
  (put SYM PROP VAL)"
    (declare (indent 0))
    (dolist (prop_ properties)
      (put (pop prop_) (pop prop_) (pop prop_))))

  (defun pew/set-hook (&rest hooks)
    "Add HOOKS.
HOOKS is a list of the form:
  (HOOK FUNC HOOK FUNC ...)
The arguments will be collected in pairs and passed to `add-hook'.
Equivalent to:"
    (declare (indent 0))
    (if (pew/oddp (length hooks))
        (error "Incomplete hooks and functions"))
    (let ((hooks_ hooks))
      (while hooks_
        (add-hook (pop hooks_) (pop hooks_)))))

  (defun pew/evenp (num)
    "Determine if NUM is odd."
    (zerop (mod num 2)))

  (defun pew/oddp (num)
    "Determine if NUM is odd."
    (not (pew/evenp num)))

  (defmacro pew/swap (a b)
    "Swap values in A and B."
    `(setq ,a (prog1 ,b (setq ,b ,a))))

  (defun pew/cycle-list (lst)
    "Put the first element in the LST to the last.
This function doesn't modify the passed-in LST."
    (if (listp lst) (append (cdr lst) (cons (car lst) nil)) nil))

  (defun pew/sync-list (lst val)
    "Cycle LST until the first element equals VAL.
Return a list with VAL as the first element or nil if no matching element found.
This function doesn't modify the passed-in LST."
    (catch 'return_
      (if (equal val (car lst))
          (throw 'return_ lst))
      ;; Fist one has been checked, skipping
      (let ((index_ 1)
            (list_ (pew/cycle-list lst))
            (max_ (length lst)))
        (while (< index_ max_)
          (if (equal val (car list_))
              (throw 'return_ list_))
          (setq list_ (pew/cycle-list list_))
          (setq index_ (1+ index_)))
        (throw 'return_ nil))))

  (defun pew/tokey (key)
    "Convert KEY to the form that can be bound with `global-set-key' or `define-key'.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
    (if (stringp key) (kbd key) key)))

;;; Debugging
(defun pew/reload-init-file ()
  "Reload the config file."
  (interactive)
  (load-file user-init-file))

(defun pew/open-init-file ()
  "Open the config file."
  (interactive)
  (find-file user-init-file))

(defun pew/expand-macro (form &optional all)
  "Expand macro the first level (or ALL) in FORM and print the expanded code."
  (let ((expanded_ (if all (macroexpand-all form) (macroexpand form))))
    (message "Expanded macro: %S" expanded_)
    expanded_))

(defun pew/keycode-to-string (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode to name: ")
  (let ((name_ (help-key-description (vector keycode) nil)))
    (message name_)))

(defun pew/buffer-full-path ()
  "Display current file path in the minibuffer."
  (interactive)
  (message buffer-file-name))

;;; Common functions and commands
(defun pew/parent-directory (path)
  "Get the parent directory of the PATH."
  (file-name-directory (directory-file-name path)))

(defvar pew/home-dir (pew/parent-directory (pew/parent-directory load-file-name))
  "The PEW configuration's home directory.
Not necessarily to be `user-emacs-directory' since this configuration can be
loaded from other places.")

(defun pew/delete-trailing-whitespaces ()
  "Clear trailing whitespaces in current buffer."
  (delete-trailing-whitespace (point-min) (point-max)))

;;; Buffers
(defvar pew/hidden-buffers
  '("^magit"
    ;; General special definitions go last
    "^ *\\*.*\\*")
  "A list of hidden buffer pattern regex.
These buffers are usually skipped and ignored from buffer list.")

(defun pew/hidden-buffer-p (name)
  "Check if the given buffer NAME is a hidden buffer.
Return t if NAME matches one of patterns defined in `pew/hidden-buffers' or nil
if there is not match."
  (catch 'return_
    (dolist (buffer-regex_ pew/hidden-buffers)
      (if (string-match buffer-regex_ name)
          (throw 'return_ t)))
    nil))

(defun pew/switch-buffer (switch-func)
  "Switch to the buffer by SWITCH-FUNC but skip hidden buffers.
Use `pew/hidden-buffer-p' to filter buffers."
  (let ((current-buffer-name_ (buffer-name)))
    (funcall switch-func)
    (while (and (pew/hidden-buffer-p (buffer-name))
                (not (string= current-buffer-name_ (buffer-name))))
      (funcall switch-func))))

(defun pew/next-buffer ()
  "Switch to the next buffer but skip hidden buffers."
  (interactive)
  (pew/switch-buffer #'next-buffer))

(defun pew/prev-buffer ()
  "Switch to the previous buffer but skip hidden buffers."
  (interactive)
  (pew/switch-buffer #'previous-buffer))

(defun pew/close-other-buffers-in-major-mode (mode)
  "Close all other buffers in major MODE but this one."
  (interactive "SMajor mode: ")
  (let ((this-buffer_ (current-buffer)))
    (dolist (buffer_ (buffer-list))
      (if (and (eq mode (buffer-local-value 'major-mode buffer_))
               (not (eq this-buffer_ buffer_)))
          (kill-buffer buffer_)))))

;;; Windows
(defun pew/pop-window-in-new-tab ()
  "Pop current window into a new tab."
  (interactive)
  (tab-bar-new-tab)
  (delete-other-windows))

(defun pew/next-window ()
  "Switch to the next window."
  (interactive)
  (other-window 1))

(defun pew/prev-window ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(defun pew/close-window ()
  "Close window and the tab if there is only one window left."
  (interactive)
  (cond ((one-window-p)
         (tab-bar-close-tab)
         (previous-window))
        (t (delete-window))))

(defun pew/scroll-other-window-page-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window))

(defun pew/scroll-other-window-page-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window-down))

(defun pew/scroll-other-window-line-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window 1))

(defun pew/scroll-other-window-line-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window -1))

(defun pew/recenter-other-window ()
  "Scroll other window one page down."
  (interactive)
  (recenter-other-window))

;;; Tabs
(defun pew/move-tab-next ()
  "Move current tab to the next."
  (interactive)
  (tab-bar-move-tab 1))

(defun pew/move-tab-prev ()
  "Move current tab to the previous."
  (interactive)
  (tab-bar-move-tab -1))

;;; Themes
(defun pew/load-theme (theme)
  "Load THEME but make sure it is the only one active."
  (interactive (list '_INTERACT_))
  (cond
   ((eq '_INTERACT_ theme)
    (call-interactively #'load-theme))
   (t (load-theme theme t)))
  ;; Disable the rest of the themes
  (if (> (length custom-enabled-themes) 1)
      (dolist (theme_ (cdr custom-enabled-themes))
        (disable-theme theme_))))

;;; Dired
(defun pew/dired-go-to ()
  "Go into the current directory/file under the cursor without creating a new buffer."
  (interactive)
  (dired-find-alternate-file))

(defun pew/dired-go-up ()
  "Go to the parent directory without creating a new buffer."
  (interactive)
  ;; Create a new buffer of the parent path
  (dired-up-directory)
  ;; Go back to the current path
  (dired-find-file)
  ;; Go up and close the current path buffer then the cursor will be on the current path
  (find-alternate-file ".."))

(defun pew/dired-close-others ()
  "Close other Dired buffers but this one."
  (interactive)
  (pew/close-other-buffers-in-major-mode 'dired-mode))

;;; Hook functions
(defun pew/term-setup ()
  "Common setup for terminal/shell modes."
  (setq-local word-wrap nil
              truncate-lines nil
              truncate-partial-width-windows nil
              show-trailing-whitespace nil
              global-hl-line-mode nil)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun pew/text-setup ()
  "Common setup for text processing modes."
  (setq-local line-move-visual t)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (display-line-numbers-mode -1)
  (electric-pair-mode -1)
  (electric-indent-mode -1))

(defun pew/reuse-window-setup ()
  "Hook for certains modes that keep spawning new windows e.g. `grep-mode'."
  (setq-local display-buffer-base-action '((display-buffer-reuse-window
                                            display-buffer-use-some-window)))
  (setq-local display-buffer-alist nil))

;;; Switch command macro
(defmacro pew/define-switch (var &optional vals)
  "Define a command to switch VAR from values VALS.
VALS is a list of values.  If VALS is not provided, VAR will be switched between
non-nil and nil."
  (let ((switch-symbol_ (intern (concat "switch/" (symbol-name var)))))
    (if (not vals)
        `(defun ,switch-symbol_ ()
           ,(format "Switch variable `%s' between non-nil and nil.
Created by `pew/define-switch'." var)
           (interactive)
           (setq ,var (not ,var))
           ;; Display status
           (message "%s: %s" ',var (if ,var "enabled" "disabled")))
      `(defun ,switch-symbol_ ()
         ,(format "Switch variable `%s' in the following values
  %S
Created by `pew/define-switch'." var vals)
         (interactive)
         (let* ((vals_ ,vals)
                (matches_ (pew/sync-list vals_ ,var)))
           (message "matches: %s" matches_)
           (if matches_
               (setq ,var (car (pew/cycle-list matches_)))
             (setq ,var (car vals_)))
           (message "%s: %s" ',var ,var))))))

(provide 'init-common)
;;; init-common.el ends here
