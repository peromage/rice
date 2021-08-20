;;; pack-dired.el --- File management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-find-file)
  :bind (("C-x C-d" . (lambda () (interactive) (find-file default-directory)))
         :map dired-mode-map
         ("RET" . dired-find-alternate-file)
         ("^" . (lambda () (interactive) (find-alternate-file "..")))
         ("DEL" . (lambda () (interactive) (find-alternate-file ".."))))
  :init
  (setq dired-listing-switches "-alFD --group-directories-first")
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (use-package dired-x
    :ensure nil))

(provide 'pack-dired)
;;; pack-dired.el ends here
