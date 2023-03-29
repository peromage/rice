;;; elpa-lang-powershell.el --- PowerShell mode -*- lexical-binding: t; -*-

;;; Commentary:
;; PowerShell major mode configuration.

;;; Code:
(use-package powershell
  :hook (powershell-mode . pew/powershell-mode/on-init)

  :config
  (defun pew/powershell-mode/on-init ()
    "`powershell-mode' initialization."
    (lsp-deferred)))

(provide 'elpa-lang-powershell)
;;; elpa-lang-powershell.el ends here
