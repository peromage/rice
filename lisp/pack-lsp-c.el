;;; pack-lsp-c.el --- C/C++ mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

(provide 'pack-lsp-c)
;;; pack-lsp-c.el ends here
