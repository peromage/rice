;;; base-eshell.el --- Eshell settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :ensure nil
  :hook (eshell-mode . pew/terminal-setup))

(provide 'base-eshell)
;;; base-eshell.el ends here
