;;; elpa-lang-csharp.el --- c# mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: csharp-mode
(pewcfg::use-package csharp-mode
  :hook (csharp-mode . pew::csharp-mode::oninit)
  :config
  (defun pew::csharp-mode::oninit ()
    "`csharp-mode' initialization."
    (lsp-deferred)))

(provide 'elpa-lang-csharp)
;;; elpa-lang-csharp.el ends here
