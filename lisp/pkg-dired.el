;;; pkg-dired.el --- Default file manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-find-file)
  :bind (("C-x C-d" . (lambda () (interactive) (find-file default-directory)))
         :map dired-mode-map
         ("RET" . dired-find-alternate-file) ;; was dired-find-file
         ("DEL" . (lambda () (interactive) (find-alternate-file ".."))) ;; was dired-unmark-backward
         ("SPC" . dired-up-directory) ;; was dired-next-line
         ;;("DEL" . dired-up-directory)
         )
  :config
  (setq dired-listing-switches "-alFD --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :ensure nil)

(provide 'pkg-dired)
;;; pkg-dired.el ends here
