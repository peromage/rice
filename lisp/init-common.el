;;; init-common.el --- Common library -*- lexical-binding: t -*-
;;; Commentary:

;; This is the PEW common library file.
;; It might be splitted into several files if it's needed in the future.
;; NOTE: This file should be loaded before any other packages.

;;; Code:

;;;; Need to be evaluated at compile-time

(eval-and-compile

  (defmacro pew/set-custom (&rest customs)
    "Set CUSTOMS variables.
CUSTOMS is a list of the form:
  (VAR VALUE VAR VALUE ...)
This macro quotes VAR, constructs a list and passes it to `pew/set-custom*'."
    (if (pew/oddp (length customs))
        (error "Incomplete variables and values"))
    (let ((args_ nil))
      (while customs
        (push `'(,(pop customs) ,(pop customs)) args_))
      `(pew/set-custom* ,@(reverse args_))))

  (defun pew/set-custom* (&rest customs)
    "Set a list of CUSTOMS.
Each custom element is a list of the form:
  (VAR VALUE [COMMENT])
The VALUE will be evaluated before passing to `customize-set-variable'."
    (dolist (custom_ customs)
      (customize-set-variable (pop custom_) (eval (pop custom_)) (pop custom_))))

  (defun pew/set-face (&rest faces)
    "Set FACES attributes.
FACES is a list of the form:
  ('(FACE ATTR VALUE ATTR VALUE ...) '(FACE ATTR VALUE ATTR VALUE ...) ...)
Each element will be passed to `set-face-attribute'.
Equivalent to:
  (set-face-attribute FACE nil ATTR VALUE ATTR VALUE ...)"
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
    (if (pew/oddp (length bindings))
        (error "Incomplete keys and definitions"))
    (while bindings
      (define-key map (pew/tokey (pop bindings)) (pop bindings))))

  (defmacro pew/create-keymap (newmap &rest bindings)
    "Create a new map with name NEWMAP and bind key BINDINGS with it.
BINDINGS is a list of the form:
  (KEY DEF KEY DEF ...)
For DEF's definition see `define-key'.
The arguments will be collected in pairs and passed to `define-key'.
Equivalent to:
  (define-key NEWMAP KEY DEF)"
    `(let ((map_ (make-sparse-keymap))
           (bindings_ (list ,@bindings)))
       (while bindings_
         (define-key map_ (pew/tokey (pop bindings_)) (pop bindings_)))
       (defvar ,newmap map_ "Keymap created by `pew/create-keymap'")))

  (defmacro pew/create-transient-command (cmd &rest bindings)
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
    (let ((map-symbol_ (intern (concat (symbol-name cmd) "-map")))
          (cmd-repeat-symbol_ (intern (concat (symbol-name cmd) "-repeat")))
          (cmd-doc-string_ "Command created by `pew/create-transient-command'"))
      `(progn
         ;; Create the map used in transient mode
         (pew/create-keymap ,map-symbol_ ,@bindings)
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
    (dolist (prop_ properties)
      (put (pop prop_) (pop prop_) (pop prop_))))

  (defun pew/set-hook (&rest hooks)
    "Add HOOKS.
HOOKS is a list of the form:
  (HOOK FUNC HOOK FUNC ...)
The arguments will be collected in pairs and passed to `add-hook'.
Equivalent to:"
    (if (pew/oddp (length hooks))
        (error "Incomplete hooks and functions"))
    (while hooks
      (add-hook (pop hooks) (pop hooks))))

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
This function doesn't modify the passed in LST."
    (if lst
        (append (cdr lst) (cons (car lst) nil))
      nil))

  (defun pew/sync-list (lst val)
    "Cycle LST until the first element equals VAL.
Return a list with VAL as the first element or nil if no matching element found."
    (catch 'return
      (if (equal val (car lst))
          (throw 'return lst))
      (let ((uplimit_ (length lst))
            (iter_ 2))
        ;; Fist one has been checked, skipping
        (setq lst (pew/cycle-list lst))
        (while (<= iter_ uplimit_)
          (if (equal val (car lst))
              (throw 'return lst))
          (setq lst (pew/cycle-list lst))
          (setq iter_ (1+ iter_)))
        (throw 'return nil))))

  (defmacro pew/toggle-var (var default)
    "Toggle variable VAR between custom value and DEFAULT value.
If VAR does not equal to DEFAULT, store value of VAR in VAR@pewstore and set
VAR to DEFAULT.  Otherwise, swap VAR and VAR@pewstore."
    `(let* ((store-symbol_ (intern ,(concat (symbol-name var) "@pewstore"))))
       (eval (list 'defvar store-symbol_ ',default "Store variable set by PEW."))
       (if (not (equal ,default ,var))
           (eval (list 'setq store-symbol_ ',var ',var ',default))
         (eval (list 'setq ',var (list 'prog1 store-symbol_ (list 'setq store-symbol_ ',var)))))))

  (defun pew/tokey (key)
    "Convert KEY to the form that can be bound with `global-set-key' or `define-key'.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
    (if (stringp key)
        (kbd key)
      key))

  )

;;;; Common functions and commands

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
    (message "Expanded macro:\n%S" expanded_)
    expanded_))

(defun pew/keycode-to-string (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode to name: ")
  (let ((name_ (help-key-description (vector keycode) nil)))
    (message name_)
    name_))

(defun pew/buffer-full-path ()
  "Display current file path in the minibuffer."
  (interactive)
  (message buffer-file-name)
  buffer-file-name)

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

;;;; Buffers

(defvar pew/hidden-buffers
  '("^magit"
    ;; General special definitions go last
    "^ *\\*.*\\*")
  "A list of hidden buffer pattern regex.
These buffers are usually skipped and ignored from buffer list.")

(defun pew/hidden-buffer-p (name)
  "Check if the given buffer NAME is a special buffer."
  (catch 'found-special
    (dolist (buffer-regex_ pew/hidden-buffers)
      (if (string-match buffer-regex_ name)
          (throw 'found-special t)))
    nil))

(defun pew/switch-buffer (switch-func)
  "Switch to the buffer by SWITCH-FUNC but skip special buffers.
SWITCH-FUNC should not take any arguments."
  (let ((current-buffer-name_ (buffer-name)))
    (funcall switch-func)
    (while (and (pew/hidden-buffer-p (buffer-name))
                (not (string= current-buffer-name_ (buffer-name))))
      (funcall switch-func))))

(defun pew/next-buffer ()
  "Switch to the next buffer but skip special buffers."
  (interactive)
  (pew/switch-buffer #'next-buffer))

(defun pew/prev-buffer ()
  "Switch to the previous buffer but skip special buffers."
  (interactive)
  (pew/switch-buffer #'previous-buffer))

(defun pew/close-other-buffers-with-major-mode (majormode)
  "Close all other buffers in MAJORMODE but thie one."
  (interactive "SMajor mode: ")
  (let ((this-buffer_ (current-buffer)))
    (dolist (buffer_ (buffer-list))
      (if (and (eq majormode (buffer-local-value 'major-mode buffer_)) (not (eq this-buffer_ buffer_)))
          (kill-buffer buffer_)))))

;;;; Windows

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

;;;; Tabs

(defun pew/move-tab-next ()
  "Move current tab to the next."
  (interactive)
  (tab-bar-move-tab 1))

(defun pew/move-tab-prev ()
  "Move current tab to the previous."
  (interactive)
  (tab-bar-move-tab -1))

;;;; Themes

(defun pew/disable-theme-list (disabled-themes)
  "Disable all themes in the DISABLED-THEMES."
  (dolist (theme_ disabled-themes)
    (disable-theme theme_)))

(defun pew/load-theme (theme)
  "Load THEME but make sure it is the only one active."
  (interactive (list '_INTERACT_))
  (cond
   ((eq '_INTERACT_ theme)
    (call-interactively #'load-theme))
   (t (load-theme theme t)))
  (if (> (length custom-enabled-themes) 1)
      (pew/disable-theme-list (cdr custom-enabled-themes))))

;;;; Builtin package utilities
;;;;; Dired

(defun pew/dired-go-to ()
  "Go into the current directory/file under the cursor without creating a new buffer."
  (interactive)
  (dired-find-alternate-file))

(defun pew/dired-go-up ()
  "Go to the parent directory without creating a new buffer."
  (interactive)
  (dired-up-directory)
  (dired-find-file)
  (find-alternate-file ".."))

(defun pew/dired-close-others ()
  "Close other Dired buffers but this one."
  (interactive)
  (pew/close-other-buffers-with-major-mode 'dired-mode))

;;;;; Hook functions

(defun pew/term-setup ()
  "Common setup for terminal on entering."
  (setq-local word-wrap nil
              truncate-lines nil
              truncate-partial-width-windows nil
              show-trailing-whitespace nil
              global-hl-line-mode nil)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun pew/reuse-window-setup ()
  "Hook for certains modes that keep spawning new windows e.g. `grep-mode'."
  (setq-local display-buffer-base-action '((display-buffer-use-some-window)))
  (setq-local display-buffer-alist nil))

;;;; Toggle and cycle commands

;; Line numbers
(defvar pew/line-number-styles '(nil t relative) "Line number styles.")
(defun pew/cycle-line-number-style ()
  "Switch line number type between relative and absolute for current buffer."
  (interactive)
  (let ((synced-style_ (pew/sync-list pew/line-number-styles display-line-numbers))
        (style-string_ "Unknown"))
    (when synced-style_
        (setq pew/line-number-styles (pew/cycle-list synced-style_))
        (setq display-line-numbers (car pew/line-number-styles)))
    (cond ((eq nil display-line-numbers)
           (setq style-string_ "Off"))
          ((eq t display-line-numbers)
           (setq style-string_ "Abusolute"))
          ((eq 'relative display-line-numbers)
           (setq style-string_ "Relative")))
    (message "Line number style: %s" style-string_)))

(defun pew/toggle-indent-tabs-mode ()
  "Switch between tab mode or space mode."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (if indent-tabs-mode
      (message "Enabled indent tabs mode")
    (message "Disabled indent tabs mode")))

(defun pew/toggle-show-trailing-whitespace ()
  "Toggle to show trailing spaces."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (if show-trailing-whitespace
      (message "Show trailing whitespaces")
    (message "Hide trailing whitespaces")))

(defun pew/toggle-visual-line-move ()
  "Toggle visual line movement."
  (interactive)
  (setq line-move-visual (not line-move-visual))
  (if line-move-visual
      (message "Visual line move on")
    (message "Visual line move off")))

(provide 'init-common)
;;; init-common.el ends here
