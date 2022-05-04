;;; base-eshell.el --- Elisp shell -*- lexical-binding: t -*-
;;; Commentary:

;; Eshell related configuration.

;;; Code:

(use-package eshell
  :ensure nil
  :hook (eshell-mode . pew/terminal-setup))

(provide 'base-eshell)
;;; base-eshell.el ends here
