;;; elpa-lang-powershell.el --- PowerShell mode  -*- lexical-binding: t -*-
;;; Commentary:

;; PowerShell script LSP support.
;; NOTE: PowerShell has to be installed first.

;;; Code:

(use-package powershell
  :hook (powershell-mode . lsp-deferred)
  ;:custom
  ;(lsp-pwsh-exe (expand-file-name ".dotnet/tools/pwsh" (getenv "HOME"))) ;; Explicitely set this if Emacs cannot find it
  )

(provide 'elpa-lang-powershell)
;;; elpa-lang-powershell.el ends here
