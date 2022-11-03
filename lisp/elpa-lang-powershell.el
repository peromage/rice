;;; elpa-lang-powershell.el --- PowerShell mode -*- lexical-binding: t -*-

;;; Commentary:
;; PowerShell major mode configuration.

;;; Code:

(use-package powershell
  :init
  (defun pew/powershell-mode/setup ()
    "PowerShell mode setup."
    (lsp-deferred))

  :hook (powershell-mode . pew/powershell-mode/setup))

(provide 'elpa-lang-powershell)
;;; elpa-lang-powershell.el ends here
