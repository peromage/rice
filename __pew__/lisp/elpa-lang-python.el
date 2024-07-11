;;; elpa-lang-python.el --- python mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: lsp-pyright
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
    (require 'dap-python)
    (lsp-deferred)))

(provide 'elpa-lang-python)
;;; elpa-lang-python.el ends here
