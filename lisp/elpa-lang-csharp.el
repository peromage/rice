;;; elpa-lang-csharp.el --- C# mode -*- lexical-binding: t -*-
;;; Commentary:

;; C# LSP support.

;;; Code:

(use-package csharp-mode
  :hook (csharp-mode . lsp-deferred))

(provide 'elpa-lang-csharp)
;;; elpa-lang-csharp.el ends here
