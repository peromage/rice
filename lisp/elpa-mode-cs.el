;;; elpa-mode-cs.el --- C# mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package csharp-mode
  :hook (csharp-mode . lsp-deferred))

(provide 'elpa-mode-cs)
;;; elpa-mode-cs.el ends here
