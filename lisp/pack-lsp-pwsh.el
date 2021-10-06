;;; pack-lsp-pwsh.el --- LSP for PowerShell  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package powershell
  :hook (powershell-mode . lsp))

(provide 'pack-lsp-pwsh)
;;; pack-lsp-pwsh.el  ends here
