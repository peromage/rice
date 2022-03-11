;;; pkg-lsp-python.el --- Python mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew/py-mode/setup ()
  "Python LSP mode setup."
  (setq indent-tabs-mode nil
        tab-width 4)
  (lsp-deferred))

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . pew/py-mode/setup)
  :custom
  (lsp-python-ms-auto-install-server t)
  (lsp-python-ms-python-executable (executable-find "python3"))
  (lsp-python-ms-python-executable-cmd "python3"))

(provide 'pkg-lsp-python)
;;; pkg-lsp-python.el ends here
