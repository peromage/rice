;;; pack-coding-lsp-py.el --- Python mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew/py-mode/setup ()
  "Python LSP mode setup."
  (require 'lsp-python-ms)
  (lsp))

(use-package lsp-python-ms
  :hook (python-mode . pew/py-mode/setup)
  :init
  (setq lsp-python-ms-auto-install-server t)
  (setq lsp-python-ms-python-executable (executable-find "python3")))

(provide 'pack-coding-lsp-py)
;;; pack-coding-lsp-py.el ends here
