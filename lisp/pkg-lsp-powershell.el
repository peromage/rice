;;; pkg-lsp-powershell.el --- PowerShell mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package powershell
  :hook (powershell-mode . lsp-deferred)
  ;;:custom
  ;; Explicitely set this if Emacs cannot find it
  ;;(lsp-pwsh-exe (expand-file-name ".dotnet/tools/pwsh" (getenv "HOME")))
  )

(provide 'pkg-lsp-powershell)
;;; pkg-lsp-powershell.el  ends here
