;;; elpa-lsp-langs.el --- LSP language supports -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lsp-java :defer t)

(use-package lsp-pyright
  :defer t
  :hook (python-mode . pew::python-mode::on-enter-lsp-mode)
  :custom
  (lsp-pyright-python-executable-cmd "python3")
  :config
  (defun pew::python-mode::on-enter-lsp-mode ()
    "`python-mode' initialization."
    (require 'lsp-pyright)
    (require 'dap-python)))

(provide 'elpa-lsp-langs)
;;; elpa-lsp-langs.el ends here
