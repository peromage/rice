;;; pkg-dired.el --- Default file manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-find-file)
  :bind (("C-x C-d" . (lambda () (interactive) (find-file default-directory)))
         :map dired-mode-map
         ;;("RET" . dired-find-alternate-file)
         ;;("DEL" . (lambda () (interactive) (find-alternate-file "..")))
         ("DEL" . dired-up-directory)
         )
  :init
  (setq dired-listing-switches "-alFD --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (use-package dired-x :ensure nil))

(provide 'pkg-dired)
;;; pkg-dired.el ends here
