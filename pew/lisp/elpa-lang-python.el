;;; elpa-lang-python.el --- Python mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Python major mode configuration.

;;; Code:
(use-package lsp-pyright
  :hook (python-mode . pew::python-mode::oninit)

  :custom
  (lsp-pyright-python-executable-cmd "python3")

  :config
  (defun pew::python-mode::oninit ()
    "`python-mode' initialization."
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 4)
    (require 'lsp-pyright)
    (require 'dap-python)
    (lsp-deferred)))

(provide 'elpa-lang-python)
;;; elpa-lang-python.el ends here
