;;; pewlib-extra.el --- Builtin packages -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; Dired
(defun pewlib::dired-go-to ()
  "Go into the target under the cursor without creating a new buffer."
  (interactive)
  (dired-find-alternate-file))

(defun pewlib::dired-go-up ()
  "Go to the parent directory without creating a new buffer."
  (interactive)
  ;; Create a new buffer of the parent path
  (dired-up-directory)
  ;; Go back to the current path
  (dired-find-file)
  ;; Go up and close the current path buffer then the cursor will be on the current path
  (find-alternate-file ".."))

(defun pewlib::dired-close-others ()
  "Close other Dired buffers but this one."
  (interactive)
  (pewlib::close-other-buffers-in-major-mode 'dired-mode))

(defun pewlib::dired-buffer-p (name)
  "Check if the given buffer NAME is a Dired buffer."
  (eq 'dired-mode (buffer-local-value 'major-mode (get-buffer name))))

;;; Eshell
(defun pewlib::eshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (if (eq 'eshell-mode major-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input))
    (error "Not an Eshell buffer")))

(provide 'pewlib-extra)
;;; pewlib-extra.el ends here
