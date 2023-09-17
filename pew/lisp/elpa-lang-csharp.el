;;; elpa-lang-csharp.el --- C# mode -*- lexical-binding: t; -*-

;;; Commentary:
;; C# major mode configuration.

;;; Code:
(use-package csharp-mode
  :hook (csharp-mode . pew::csharp-mode::oninit)

  :config
  (defun pew::csharp-mode::oninit ()
    "`csharp-mode' initialization."
    (lsp-deferred)))

(provide 'elpa-lang-csharp)
;;; elpa-lang-csharp.el ends here
