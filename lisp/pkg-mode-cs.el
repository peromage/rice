;;; pkg-mode-cs.el --- C# mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package csharp-mode
  :ensure t
  :hook (csharp-mode . lsp-deferred))

(provide 'pkg-mode-cs)
;;; pkg-mode-cs.el ends here
