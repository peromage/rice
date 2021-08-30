;;; pack-lsp-c.el --- C/C++ mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew-pack/lsp-c-setup ()
  "Initialization for C mode."
  (setq c-basic-offset tab-width
        ;; Prevent Clangd from inserting headers itself
        lsp-clients-clangd-args
        '("-j=2"
          "--background-index"
          "--clang-tidy"
          "--completion-style=bundled"
          "--pch-storage=memory"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (lsp))

(defun pew-pack/lsp-cpp-setup ()
  "Initialization for C++ mode."
  (c-set-offset 'innamespace [0])
  (pew-pack/lsp-c-setup))

(add-hook 'c-mode-hook #'pew-pack/lsp-c-setup)
(add-hook 'c++-mode-hook #'pew-pack/lsp-cpp-setup)

(provide 'pack-lsp-c)
;;; pack-lsp-c.el ends here
