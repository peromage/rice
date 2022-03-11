;;; pkg-lsp-powershell.el --- PowerShell mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package powershell
  :ensure t
  :hook (powershell-mode . lsp-deferred)
  ;:custom
  ;(lsp-pwsh-exe (expand-file-name ".dotnet/tools/pwsh" (getenv "HOME"))) ;; Explicitely set this if Emacs cannot find it
  )

(provide 'pkg-lsp-powershell)
;;; pkg-lsp-powershell.el  ends here
