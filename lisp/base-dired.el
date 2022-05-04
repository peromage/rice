;;; base-dired.el --- Default file manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Dired functions
;;------------------------------------------------------------------------------

(defun pew/dired/go-to ()
  "Go into the current directory/file under the cursor without creating a new buffer."
  (interactive)
  (dired-find-alternate-file))

(defun pew/dired/go-up ()
  "Go to the parent directory without creating a new buffer."
  (interactive)
  (dired-up-directory)
  (dired-find-file)
  (find-alternate-file ".."))

(defun pew/dired/go-to-default-directory ()
  "Go to the directory where the current file resides."
  (interactive)
  (find-file default-directory))

;;------------------------------------------------------------------------------
;; Dired
;;------------------------------------------------------------------------------

(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-find-file)
  :bind (("C-x C-d" . pew/dired/go-to-default-directory)
         :map dired-mode-map
         ("RET" . pew/dired/go-to) ;; was dired-find-file
         ("DEL" . pew/dired/go-up) ;; was dired-unmark-backward
         ;("SPC" . dired-up-directory) ;; was dired-next-line
         ;("DEL" . dired-up-directory)
         )
  :custom
  (dired-listing-switches "-alFD --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :ensure nil
  :after dired)

(provide 'base-dired)
;;; base-dired.el ends here
