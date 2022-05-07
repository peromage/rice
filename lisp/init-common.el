;;; init-common.el --- Common library -*- lexical-binding: t -*-
;;; Commentary:

;; This is the PEW common library file.
;; It might be splitted into several files if it's needed in the future.
;; NOTE: This file should be loaded before any other packages.

;;; Code:

;;;; Debug utilities

(defun pew/reload-initel ()
  "Reload the config file."
  (interactive)
  (load-file user-init-file))

(defun pew/open-initel ()
  "Open the config file."
  (interactive)
  (find-file user-init-file))

(defun pew/expand-macro (form)
  "Expand macro in FORM and print the expanded code (no evaluation)."
  (message "Expanded macro:\n%S" (macroexpand form)))

;;;; Common utilities

(defun pew/find-keyname (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode to name: ")
  (message "%s" (help-key-description (vector keycode) nil)))

(defun pew/tokey (key)
  "Convert KEY to the form that can be bound with `global-set-key' or `define-key'.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
  (if (vectorp key)
      key
    (kbd key)))

(defun pew/show-file-path ()
  "Display current file path in the minibuffer."
  (interactive)
  (message buffer-file-name))

(defun pew/get-parent-directory (path)
  "Get the parent directory of the PATH."
    (file-name-directory (directory-file-name path)))

(defvar pew/home-dir (pew/get-parent-directory (pew/get-parent-directory load-file-name))
  "The PEW configuration's home directory.
Not necessarily to be `user-emacs-directory' since this configuration can be
loaded from other places.")

(defun pew/open-cwd ()
  "Go to the directory where the current file resides."
  (interactive)
  (find-file default-directory))

;;;; Toggles

(defun pew/toggle-line-number-type ()
  "Switch line number type between relative and absolute for current buffer."
  (interactive)
  (cond ((eq 'relative display-line-numbers)
         (setq display-line-numbers t)
         (message "Set line numbers to normal"))
        (t
         (setq display-line-numbers 'relative)
         (message "Set line numbers to relative"))))

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

;;;; Buffer utilities

(defvar pew/special-buffers '("\\` "
                              "\\`\\*"
                              "\\`magit-"
                              "\\`magit:")
  "A regex list of special buffer patterns.
Special buffers are usually skipped and ignored from buffer list.")

(defun pew/special-buffer-p (name)
  "Check if the given buffer NAME is a special buffer."
  (catch 'found-special
    (dolist (buffer-regex pew/special-buffers)
      (if (string-match buffer-regex name)
          (throw 'found-special t)))
    nil))

(defun pew/switch-buffer (switch-func)
  "Switch to the buffer by SWITCH-FUNC but skip special buffers.
SWITCH-FUNC should not take any arguments."
  (let ((current-buffer-name (buffer-name)))
    (funcall switch-func)
    (while (and (pew/special-buffer-p (buffer-name))
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

;;;; Window utilities

(defun pew/pop-window-in-new-tab ()
  "Pop current window into a new tab."
  (interactive)
  (tab-bar-new-tab)
  (delete-other-windows))

;;;; Tab utilities

(defun pew/move-tab-next ()
  "Move current tab to the next."
  (interactive)
  (tab-bar-move-tab 1))

(defun pew/move-tab-prev ()
  "Move current tab to the previous."
  (interactive)
  (tab-bar-move-tab -1))

;;;; Theme utilities

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

;;;; Macros

(defmacro pew/set-custom (&rest customs)
  "A helper macro that set CUSTOMS from `customize' interface.
CUSTOMS is an alist in the form of:
  (pew/set-custom option1 value1
                  option2 value2
                  ...)
The values will be evaluated before passing to `customize-set-variable'.
The result is equivalent to:
  (customize-set-variable 'option1 value1)
  (customize-set-variable 'option2 value2)
  ..."
  (if (cl-oddp (length customs))
      (error "Incomplete pairs!"))
  (let ((result '(progn)))
    (while customs
      ;; Prefer using `push' than `add-to-list' since the later checks elements
      (push `(customize-set-variable ',(car customs) ,(cadr customs)) result)
      (setq customs (cddr customs)))
    ;; Since `push' causes configuration lines being read in a reversed way we
    ;; need to fix it to the right order
    ;; Prefer using `nreverse' because it's way faster then `reverse'
    (nreverse result)))

(defmacro pew/set-face (&rest faces)
  "A helper macro that set FACES.
FACES is a list in the form of:
  (pew/set-face face1 (attr1_1 value1_1 attr1_2 value1_2 ...)
                face2 (attr2_1 value2_1 attr2_2 value2_2 ...)
                ...)
For the attribute plist see `defface'.
The result is equivalent to:
  (set-face-attribute 'face1 nil attr1a value1a attr1b value1b ...)
  (set-face-attribute 'face2 nil attr2a value2a attr2b value2b ...)
  ..."
  (if (cl-oddp (length faces))
      (error "Incomplete pairs!"))
  (let ((result '(progn)))
    (while faces
      (push `(set-face-attribute ',(car faces) nil ,@(cadr faces)) result)
      (setq faces (cddr faces)))
    (nreverse result)))

(defmacro pew/set-key (&rest keys)
  "A helper macro that binds KEYS globally.
KEYS is an alist in the form of:
  (pew/set-key (\"binding1\" . func1)
               ([binding2] . func2)
               ...)
See `global-set-key' for keybinding help.
The result is equivalent to:
  (global-set-key \"binding1\" #'func1)
  (global-set-key [binding2] #'func2)
  ..."
  `(progn ,@(mapcar (lambda (bind)
                      `(global-set-key ,(pew/tokey (car bind)) #',(cdr bind)))
                    keys)))

(defmacro pew/set-map (&rest maps)
  "A helper macro that set MAPS keybindings.
MAPS is a list in the form of:
  (pew/set-map (map1 (binding1a . func1a)
                     (binding1b . func1b))
               (map2 (binding2a . func2a)
                     (binding2b . func2b))
               ...)
See `define-key'.
The result is equivalent to:
  (define-key map1 binding1a func1a)
  (define-key map1 binding1b func1b)
  (define-key map2 binding2a func2a)
  (define-key map2 binding2b func2b)
  ..."
  `(progn ,@(mapcan (lambda (map)
                      (mapcar (lambda (bind)
                                `(define-key ,(car map) ,(pew/tokey (car bind)) #',(cdr bind)))
                              (cdr map)))
                    maps)))

(defmacro pew/set-enabled (&rest options)
  "A helper macro that enables OPTIONS that are disabled by default.
OPTIONS is a list in the form of:
  (pew/set-enabled command1 command2 ...)
The result is equivalent to:
  (put 'command1 'disabled nil)
  (put 'command2 'disabled nil)
  ..."
  `(progn ,@(mapcar (lambda (cmd)
                      `(put ',cmd 'disabled nil))
                    options)))

(defmacro pew/set-hook (&rest hooks)
  "Add HOOKS.
HOOKS is an alist in the form of:
  (pew/set-hook (hook1 . func1)
                (hook2 . func2)
                ...)
See `add-hook'.
The result is equivalent to:
  (add-hook 'hook1 #'func1)
  (add-hook 'hook2 #'func2)
  ..."
  `(progn ,@(mapcar (lambda (hook)
                      `(add-hook ',(car hook) #',(cdr hook)))
                    hooks)))

(provide 'init-common)
;;; init-common.el ends here
