;;; init-common.el --- Common library -*- lexical-binding: t -*-
;;; Commentary:

;; This is the PEW common library file.
;; It might be splitted into several files if it's needed in the future.
;; NOTE: This file should be loaded before any other packages.

;;; Code:

;;;; Debug utilities

;;;###autoload
(defun pew/reload-initel ()
  "Reload the config file."
  (interactive)
  (load-file user-init-file))

;;;###autoload
(defun pew/open-initel ()
  "Open the config file."
  (interactive)
  (find-file user-init-file))

;;;###autoload
(defun pew/expand-macro (form)
  "Expand macro in FORM and print the expanded code (no evaluation)."
  (message "Expanded macro:\n%S" (macroexpand form)))

;;;; Common utilities

;;;###autoload
(defun pew/evenp (num)
  "Determine if NUM is odd."
  (zerop (mod num 2)))

;;;###autoload
(defun pew/oddp (num)
  "Determine if NUM is odd."
  (not (pew/evenp num)))

;;;###autoload
(defun pew/find-keyname (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode to name: ")
  (message "%s" (help-key-description (vector keycode) nil)))

;;;###autoload
(defun pew/tokey (key)
  "Convert KEY to the form that can be bound with `global-set-key' or `define-key'.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
  (if (vectorp key)
      key
    (kbd key)))

;;;###autoload
(defun pew/show-file-path ()
  "Display current file path in the minibuffer."
  (interactive)
  (message buffer-file-name))

;;;###autoload
(defun pew/get-parent-directory (path)
  "Get the parent directory of the PATH."
    (file-name-directory (directory-file-name path)))

;;;###autoload
(defvar pew/home-dir (pew/get-parent-directory (pew/get-parent-directory load-file-name))
  "The PEW configuration's home directory.
Not necessarily to be `user-emacs-directory' since this configuration can be
loaded from other places.")

;;;###autoload
(defun pew/open-cwd ()
  "Go to the directory where the current file resides."
  (interactive)
  (find-file default-directory))

;;;###autoload
(defun pew/delete-trailing-whitespaces ()
  "Clear trailing whitespaces in current buffer."
  (delete-trailing-whitespace (point-min) (point-max)))

;;;; Toggles

;;;###autoload
(defun pew/toggle-line-number-type ()
  "Switch line number type between relative and absolute for current buffer."
  (interactive)
  (cond ((eq 'relative display-line-numbers)
         (setq display-line-numbers t)
         (message "Set line numbers to normal"))
        (t
         (setq display-line-numbers 'relative)
         (message "Set line numbers to relative"))))

;;;###autoload
(defun pew/toggle-indent-tabs-mode ()
  "Switch between tab mode or space mode."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (if indent-tabs-mode
      (message "Enabled indent tabs mode")
    (message "Disabled indent tabs mode")))

;;;###autoload
(defun pew/toggle-show-trailing-whitespace ()
  "Toggle to show trailing spaces."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (if show-trailing-whitespace
      (message "Show trailing whitespaces")
    (message "Hide trailing whitespaces")))

;;;###autoload
(defun pew/toggle-visual-line-move ()
  "Toggle visual line movement."
  (interactive)
  (setq line-move-visual (not line-move-visual))
  (if line-move-visual
      (message "Visual line move on")
    (message "Visual line move off")))

;;;; Buffer utilities

;;;###autoload
(defvar pew/special-buffers '("\\`magit"
                              ;; General special definitions go last
                              "\\` *\\*.*\\*")
  "A regex list of special buffer patterns.
Special buffers are usually skipped and ignored from buffer list.")

(defun pew/special-buffer-p (name)
  "Check if the given buffer NAME is a special buffer."
  (catch 'found-special
    (dolist (buffer-regex pew/special-buffers)
      (if (string-match buffer-regex name)
          (throw 'found-special t)))
    nil))

;;;###autoload
(defun pew/switch-buffer (switch-func)
  "Switch to the buffer by SWITCH-FUNC but skip special buffers.
SWITCH-FUNC should not take any arguments."
  (let ((current-buffer-name (buffer-name)))
    (funcall switch-func)
    (while (and (pew/special-buffer-p (buffer-name))
                (not (string= current-buffer-name (buffer-name))))
      (funcall switch-func))))

;;;###autoload
(defun pew/next-buffer ()
  "Switch to the next buffer but skip special buffers."
  (interactive)
  (pew/switch-buffer #'next-buffer))

;;;###autoload
(defun pew/prev-buffer ()
  "Switch to the previous buffer but skip special buffers."
  (interactive)
  (pew/switch-buffer #'previous-buffer))

;;;###autoload
(defun pew/close-other-buffers-with-major-mode (majormode)
  "Close all other buffers in MAJORMODE but thie one."
  (interactive "SMajor mode: ")
  (let ((this-buf (current-buffer)))
    (dolist (buf (buffer-list))
      (if (and (eq majormode (buffer-local-value 'major-mode buf)) (not (eq this-buf buf)))
          (kill-buffer buf)))))

;;;; Window utilities

;;;###autoload
(defun pew/pop-window-in-new-tab ()
  "Pop current window into a new tab."
  (interactive)
  (tab-bar-new-tab)
  (delete-other-windows))

;;;; Tab utilities

;;;###autoload
(defun pew/move-tab-next ()
  "Move current tab to the next."
  (interactive)
  (tab-bar-move-tab 1))

;;;###autoload
(defun pew/move-tab-prev ()
  "Move current tab to the previous."
  (interactive)
  (tab-bar-move-tab -1))

;;;; Theme utilities

;;;###autoload
(defun pew/disable-theme-list (disabled-themes)
  "Disable all themes in the DISABLED-THEMES."
  (dolist (theme disabled-themes)
    (disable-theme theme)))

;;;###autoload
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

;;;###autoload
(defun pew/dired-go-to ()
  "Go into the current directory/file under the cursor without creating a new buffer."
  (interactive)
  (dired-find-alternate-file))

;;;###autoload
(defun pew/dired-go-up ()
  "Go to the parent directory without creating a new buffer."
  (interactive)
  (dired-up-directory)
  (dired-find-file)
  (find-alternate-file ".."))

;;;###autoload
(defun pew/dired-close-others ()
  "Close other Dired buffers but this one."
  (interactive)
  (pew/close-other-buffers-with-major-mode 'dired-mode))

;;;;; Terminals

;;;###autoload
(defun pew/term-setup ()
  "Common setup for terminal on entering."
  (setq-local word-wrap nil
              truncate-lines nil
              truncate-partial-width-windows nil
              global-hl-line-mode nil)
  (hl-line-mode -1)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

;;;; Macros

(defmacro pew/set-custom (&rest customs)
  "A helper macro that sets CUSTOMS from `customize' interface in `setq' fashion.
CUSTOMS is an alist of the form:
  (pew/set-custom option1 value1
                  option2 value2
                  ...)
The values will be evaluated before passing to `customize-set-variable'.
The result is equivalent to:
  (customize-set-variable 'option1 value1)
  (customize-set-variable 'option2 value2)
  ..."
  (if (pew/oddp (length customs))
      (error "Incomplete option and value pairs"))
  (let ((result '(progn)))
    (while customs
      ;; Prefer using `push' than `add-to-list' since the later checks elements
      (push `(customize-set-variable ',(pop customs) ,(pop customs)) result))
    ;; Since `push' causes configuration lines being read in a reversed way we
    ;; need to fix it to the right order
    ;; NOTE: `nreverse' seems to be faster but it doesn't work properly sometims
    (reverse result)))

(defmacro pew/set-face (&rest faces)
  "A helper macro that sets FACES.
FACES is a list of the form:
  (pew/set-face 'face1 '(attr1_1 value1_1 attr1_2 value1_2 ...)
                'face2 '(attr2_1 value2_1 attr2_2 value2_2 ...)
                ...)
For the attribute plist see `defface'.
The result is equivalent to:
  (set-face-attribute 'face1 nil attr1a value1a attr1b value1b ...)
  (set-face-attribute 'face2 nil attr2a value2a attr2b value2b ...)
  ..."
  (if (pew/oddp (length faces))
      (error "Incomplete face and attribute list pairs"))
  (let ((result '(progn)))
    (while faces
      (push `(set-face-attribute ,(car faces) nil ,@(cadr (cadr faces))) result)
      (setq faces (cddr faces)))
    (reverse result)))

(defmacro pew/set-key (map &rest bindings)
  "A helper macro that binds BINDINGS in MAP.
BINDINGS is a list of the form:
  (pew/set-key map \"binding1\" #'def1
                   [binding2] def2
                   ...)
See `define-key' for def definitions.
The result is equivalent to:
  (define-key map \"binding1\" #'def1)
  (define-key map [binding2] def2)
  ..."
  (if (pew/oddp (length bindings))
      (error "Incomplete key and definition pairs"))
  (let ((result '(progn)))
    (while bindings
      (push `(define-key ,map ,(pew/tokey (pop bindings)) ,(pop bindings)) result))
    (reverse result)))

(defmacro pew/set-enabled (&rest commands)
  "A helper macro that enables COMMANDS that are disabled by default.
COMMANDS is a list of the form:
  (pew/set-enabled 'command1 'command2 ...)
The result is equivalent to:
  (put 'command1 'disabled nil)
  (put 'command2 'disabled nil)
  ..."
  `(progn
     ,@(mapcar
        (lambda (cmd)
          `(put ,cmd 'disabled nil))
        commands)))

(defmacro pew/set-hook (&rest hooks)
  "Add function calls to HOOKS.
HOOKS is a list of the form:
  (pew/set-hook hook1 #'func1)
                hook2 #'func2)
                ...)
See `add-hook'.
The result is equivalent to:
  (add-hook 'hook1 #'func1)
  (add-hook 'hook2 #'func2)
  ..."
  (let ((result '(progn)))
    (while hooks
      (push `(add-hook ',(pop hooks) ,(pop hooks)) result))
    (reverse result)))

(provide 'init-common)
;;; init-common.el ends here
