;;; init-lib.el --- Utilities -*- lexical-binding: t -*-
;;; Commentary:

;; This is the pew library file.
;; I might split this into multiple files if there are too many shared functions
;; in the future.

;;; Code:

;;------------------------------------------------------------------------------
;; Shared variables
;;------------------------------------------------------------------------------

(setq pew/special-buffers
      '("\\` "
        "\\`\\*"
        "\\`magit-"
        "\\`magit:"))

;;------------------------------------------------------------------------------
;; Shared functions
;;------------------------------------------------------------------------------

;; Debug functions
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

(defun pew/global-set-key (keybindings)
  "Globally bind keys defined in the alist KEYBINDINGS.
The alist KEYBINDINGS should be something like:
  '((\"key strokes\" . command)
    ([key strokes] . command))"
  (dolist (binding keybindings)
    (let ((keys (car binding))
          (cmd (cdr binding)))
      (if (vectorp keys)
          (global-set-key keys cmd)
        (global-set-key (kbd keys) cmd)))))

(defun pew/show-file-path ()
  "Display current file path in the minibuffer."
  (interactive)
  (message buffer-file-name))

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

(defun pew/load-if-exists (file)
  "Load the Emacs script FILE if it exists."
  (when (file-exists-p file)
    (load file)))

(defun pew/toggle-line-number-type ()
  "Switch line number type between relative and absolute for current buffer."
  (interactive)
  (cond ((eq 'relative display-line-numbers-type)
         (setq-local display-line-numbers-type t)
         (display-line-numbers-mode 1))
        (t
         (setq-local display-line-numbers-type 'relative)
         (display-line-numbers-mode 1))))

(defun pew/global-toggle-line-number-type ()
  "Switch line number type between relative and absolute globally."
  (interactive)
  (cond ((eq 'relative display-line-numbers-type)
         (setq-default display-line-numbers-type t)
         (global-display-line-numbers-mode 1))
        (t
         (setq-default display-line-numbers-type 'relative)
         (global-display-line-numbers-mode 1))))

(provide 'init-lib)
;;; init-lib.el ends here
