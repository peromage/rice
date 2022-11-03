;;; elpa-lang-python.el --- Python mode -*- lexical-binding: t -*-

;;; Commentary:
;; Python major mode configuration.

;;; Code:

(use-package lsp-pyright
  :init
  (defun pew/python-mode/setup ()
    "Python LSP mode setup."
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 4)
    (require 'lsp-pyright)
    (require 'dap-python)
    (lsp-deferred))

  :hook (python-mode . pew/python-mode/setup)
  :custom
  (lsp-pyright-python-executable-cmd "python3"))

(provide 'elpa-lang-python)
;;; elpa-lang-python.el ends here
