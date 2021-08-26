;;; pack-lsp-c.el --- C/C++ mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew/c-lsp-setup ()
  "Initialization for C/C++ mode."
  (setq c-basic-offset tab-width)
  (lsp))

(add-hook 'c-mode-hook #'pew/c-lsp-setup)
(add-hook 'c++-mode-hook #'pew/c-lsp-setup)

(provide 'pack-lsp-c)
;;; pack-lsp-c.el ends here
