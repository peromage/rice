;;; pkg-eshell.el --- Eshell settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :hook (eshell-mode . pew/terminal-setup))

(provide 'pkg-eshell)
;;; pkg-eshell.el ends here
