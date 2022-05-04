;;; elpa-mode-python.el --- Python mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew/python-mode/setup ()
  "Python LSP mode setup."
  (setq indent-tabs-mode nil
        tab-width 4)
  (lsp-deferred))

(use-package lsp-python-ms
  :hook (python-mode . pew/python-mode/setup)
  :custom
  (lsp-python-ms-auto-install-server t)
  (lsp-python-ms-python-executable (executable-find "python3"))
  (lsp-python-ms-python-executable-cmd "python3"))

(provide 'elpa-mode-python)
;;; elpa-mode-python.el ends here
