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
    ;; No need to check the number of arguments. If the list length is odd then
    ;; the last VAR will be set to nil
    (let ((args nil))
      (while customs
        (push `'(,(pop customs) ,(pop customs)) args))
      `(pew/set-custom* ,@(reverse args))))

  (defun pew/set-custom* (&rest customs)
    "Set a list of CUSTOMS.
Each custom element is a list of the form:
  (VAR VALUE [COMMENT])
The VALUE will be evaluated before passing to `customize-set-variable'."
    (dolist (cus customs)
      (customize-set-variable (pop cus) (eval (pop cus)) (pop cus))))

  (defun pew/set-face (&rest faces)
    "Set FACES attributes.
FACES is a list of the form:
  (FACE '(ATTR VALUE ATTR VALUE ...) FACE '(ATTR VALUE ATTR VALUE ...) ...)
The arguments will be collected in pairs and passed to `set-face-attribute'.
Equivalent to:
  (set-face-attribute FACE nil ATTR VALUE ATTR VALUE ...)"
    (if (pew/oddp (length faces))
        (error "Incomplete faces and attributes"))
    (while faces
      (apply 'set-face-attribute (pop faces) nil (pop faces))))

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

  (defun pew/create-map (newmap &rest bindings)
    "Create a new map with name NEWMAP and bind key BINDINGS with it.
BINDINGS is a list of the form:
  (KEY DEF KEY DEF ...)
For DEF's definition see `define-key'.
The arguments will be collected in pairs and passed to `define-key'.
Equivalent to:
  (define-key NEWMAP KEY DEF)"
    (if (pew/oddp (length bindings))
        (error "Incomplete keys and definitions"))
    (let ((map (make-sparse-keymap)))
      (while bindings
        (define-key map (pew/tokey (pop bindings)) (pop bindings)))
      (eval `(defvar ,newmap ',map "Map created by `pew/create-map'"))))

  (defun pew/create-repeat-map (name &rest bindings)
    "Create a repeat map for a dummy command called NAME and its key BINDINGS.
This macro will create an interactive command NAME and the associated map with
name NAME-repeat-map. Then set `repeat-map' property to the command after.
BINDINGS is a list of the form:
  (KEY DEF KEY DEF ...)
For DEF's definition see `define-key'. "
    (let ((map-symbol (intern (concat (symbol-name name) "-repeat-map"))))
      ;; Create the map used in `repeat-mode'
      (apply 'pew/create-map map-symbol bindings)
      ;; Define the dummy command to invoke the previous repeat map
      (eval
       `(defun ,name ()
          "Dummy command created by `pew/create-repeat-map'"
          (interactive)))
      ;; Enable repeat map for the dummy command whenever it's called
      (put name 'repeat-map map-symbol)))

  (defun pew/set-property (&rest properties)
    "Set PROPERTIES for symbols.
Each element in PROPERTIES is of the form:
  (SYM PROP VAL)
The arguments will be passed to `put' one by one.
Equivalent to:
  (put SYM PROP VAL)"
    (dolist (prop properties)
      (put (pop prop) (pop prop) (pop prop))))

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
      (let ((uplimit (length lst))
            (iter 2))
        ;; Fist one has been checked, skipping
        (setq lst (pew/cycle-list lst))
        (while (<= iter uplimit)
          (if (equal val (car lst))
              (throw 'return lst))
          (setq lst (pew/cycle-list lst))
          (setq iter (1+ iter)))
        (throw 'return nil))))

  (defmacro pew/toggle-var (var default)
    "Toggle variable VAR between custom value and DEFAULT value.
If VAR does not equal to DEFAULT, store value of VAR in VAR@pewstore and set
VAR to DEFAULT.  Otherwise, swap VAR and VAR@pewstore."
    `(let* ((store-symbol (intern ,(concat (symbol-name var) "@pewstore"))))
       (eval (list 'defvar store-symbol ',default "Store variable set by PEW."))
       (if (not (equal ,default ,var))
           (eval (list 'setq store-symbol ',var ',var ',default))
         (eval (list 'setq ',var (list 'prog1 store-symbol (list 'setq store-symbol ',var)))))))

  (defun pew/tokey (key)
    "Convert KEY to the form that can be bound with `global-set-key' or `define-key'.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
    (if (vectorp key)
        key
      (kbd key)))

  )

;;;; Common functions and commands

(defun pew/reload-initel ()
  "Reload the config file."
  (interactive)
  (load-file user-init-file)
  (message "Reloaded user init file: %s" user-init-file)
  user-init-file)

(defun pew/open-initel ()
  "Open the config file."
  (interactive)
  (find-file user-init-file)
  (message "Open user init file: %s" user-init-file)
  user-init-file)

(defun pew/expand-macro (form &optional all)
  "Expand macro the first level (or ALL) in FORM and print the expanded code."
  (let ((expanded (if all (macroexpand-all form) (macroexpand form))))
    (message "Expanded macro:\n%S" expanded)
    expanded))

(defun pew/keycode-to-string (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode to name: ")
  (let ((string (help-key-description (vector keycode) nil)))
    (message string)
    string))

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

(defun pew/open-cwd ()
  "Go to the directory where the current file resides."
  (interactive)
  (find-file default-directory))

(defun pew/delete-trailing-whitespaces ()
  "Clear trailing whitespaces in current buffer."
  (delete-trailing-whitespace (point-min) (point-max)))

;;;; Buffers

(defvar pew/hidden-buffers
  '("\\`magit"
    ;; General special definitions go last
    "\\` *\\*.*\\*")
  "A list of hidden buffer pattern regex.
These buffers are usually skipped and ignored from buffer list.")

(defun pew/hidden-buffer-p (name)
  "Check if the given buffer NAME is a special buffer."
  (catch 'found-special
    (dolist (buffer-regex pew/hidden-buffers)
      (if (string-match buffer-regex name)
          (throw 'found-special t)))
    nil))

(defun pew/switch-buffer (switch-func)
  "Switch to the buffer by SWITCH-FUNC but skip special buffers.
SWITCH-FUNC should not take any arguments."
  (let ((current-buffer-name (buffer-name)))
    (funcall switch-func)
    (while (and (pew/hidden-buffer-p (buffer-name))
                (not (string= current-buffer-name (buffer-name))))
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
  (let ((this-buf (current-buffer)))
    (dolist (buf (buffer-list))
      (if (and (eq majormode (buffer-local-value 'major-mode buf)) (not (eq this-buf buf)))
          (kill-buffer buf)))))

;;;; Windows

(defun pew/pop-window-in-new-tab ()
  "Pop current window into a new tab."
  (interactive)
  (tab-bar-new-tab)
  (delete-other-windows))

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
  (dolist (theme disabled-themes)
    (disable-theme theme)))

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
  (let ((synced-style (pew/sync-list pew/line-number-styles display-line-numbers))
        (style-string "Unknown"))
    (when synced-style
        (setq pew/line-number-styles (pew/cycle-list synced-style))
        (setq display-line-numbers (car pew/line-number-styles)))
    (cond ((eq nil display-line-numbers)
           (setq style-string "Off"))
          ((eq t display-line-numbers)
           (setq style-string "Abusolute"))
          ((eq 'relative display-line-numbers)
           (setq style-string "Relative")))
    (message "Line number style: %s" style-string)))

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
