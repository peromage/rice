;;; pack-lsp-python.el --- Python mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t)
  (setq lsp-python-ms-python-executable (executable-find "python3"))
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(provide 'pack-lsp-python)
;;; pack-lsp-python.el ends here
