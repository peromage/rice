;;; elpa-lsp-langs.el --- LSP language supports -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(pewcfg::use-package lsp-java :defer t)

(pewcfg::use-package lsp-pyright
  :defer t
  :hook (python-mode . pew::python-mode::oninit)
  :custom
  (lsp-pyright-python-executable-cmd "python3")

  :config
  (defun pew::python-mode::oninit ()
    "`python-mode' initialization."
    (setq-local indent-tabs-mode nil
                tab-width 4)
    (require 'lsp-pyright)
    (require 'dap-python)))

(provide 'elpa-lsp-langs)
;;; elpa-lsp-langs.el ends here
