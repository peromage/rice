;;; pack-lsp-c.el --- C/C++ mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew-pack/lsp-c-setup ()
  "Initialization for C/C++ mode."
  (setq c-basic-offset tab-width)
  (c-set-offset 'innamespace [0])
  (lsp))

(add-hook 'c-mode-hook #'pew-pack/lsp-c-setup)
(add-hook 'c++-mode-hook #'pew-pack/lsp-c-setup)

(provide 'pack-lsp-c)
;;; pack-lsp-c.el ends here
