;;; init-lib.el --- Utilities -*- lexical-binding: t -*-
;;; Commentary:

;; This is the pew library file.
;; I might split this into multiple files if there are too many shared functions
;; in the future.

;;; Code:

;;==============================================================================
;; Global variables
;;==============================================================================

(setq pew/special-buffers
      '("\\` "
        "\\`\\*"
        "\\`magit-"
        "\\`magit:"))

;;==============================================================================
;; Global functions
;;==============================================================================

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

(defun pew/start-emacs-daemon ()
  "Start daemon if it does not exist."
  (require 'server)
  (unless (server-running-p)
    (message "[pew] Starting Emacs daemon...")
    (server-start)))

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

(provide 'init-lib)
;;; init-lib.el ends here
