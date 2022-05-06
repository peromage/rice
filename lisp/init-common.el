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

(defun pew/find-keyname (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode to name: ")
  (message "%s" (help-key-description (vector keycode) nil)))

;;;; Common utilities

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
  "A helper macro to set options from `customize' interface.
This macro takes a list CUSTOMS of custom options in form:
  (pew/set-custom option1 value1
                  option2 value2
                  option3 value3
                  ...)
The values will be evaluated before passing to `customize-set-variable'."
  `(let ((_ ',customs))
     (while _
       (customize-set-variable (car _) (eval (cadr _)))
       (setq _ (cddr _)))))

(defmacro pew/set-key (&rest keys)
  "Globally bind keys from KEYS alist.
This macro takes in form:
  (pew/set-key (\"binding1\" . func1)
               (\"binding2\" . func1)
               ...)"
  `(dolist (_ ',keys)
     (let ((k (car _))
           (c (cdr _)))
       (unless (vectorp k)
         (setq k (kbd k)))
       (global-set-key k c))))

(defmacro pew/set-enabled (&rest options)
  "Enable OPTIONS that are disabled by default.
OPTIONS is a list in form:
  (scroll-left list-threads list-timers ...)"
  `(dolist (_ ',options)
     (put _ 'disabled nil)))

(provide 'init-common)
;;; init-common.el ends here
