;;; elpa-lang-csharp.el --- C# mode -*- lexical-binding: t -*-

;;; Commentary:
;; C# major mode configuration.

;;; Code:

(use-package csharp-mode
  :init
  (defun pew/csharp-mode/setup ()
    "Python mode setup."
    (lsp-deferred))

  :hook (csharp-mode . pew/csharp-mode/setup))

(provide 'elpa-lang-csharp)
;;; elpa-lang-csharp.el ends here
