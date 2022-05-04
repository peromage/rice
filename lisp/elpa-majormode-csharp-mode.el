;;; elpa-majormode-csharp-mode.el --- C# mode -*- lexical-binding: t -*-
;;; Commentary:

;; C# LSP support.

;;; Code:

(use-package csharp-mode
  :hook (csharp-mode . lsp-deferred))

(provide 'elpa-majormode-csharp-mode)
;;; elpa-majormode-csharp-mode.el ends here
