;;; pkg-lsp-powershell.el --- PowerShell mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package powershell
  :hook (powershell-mode . lsp-deferred))

(provide 'pkg-lsp-powershell)
;;; pkg-lsp-powershell.el  ends here
