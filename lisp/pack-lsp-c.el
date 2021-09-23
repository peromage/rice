;;; pack-lsp-c.el --- C/C++ mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; Functions and variables
;;==============================================================================

(defun pew-lsp/c-setup ()
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
          "--header-insertion-decorators=0"
          "--suggest-missing-includes"
          "--all-scopes-completion")
        lsp-enable-on-type-formatting nil
        adaptive-fill-mode nil)
  (lsp)
  ;; Post LSP mode settings
  (electric-indent-mode 1))

(defun pew-lsp/cpp-setup ()
  "Initialization for C++ mode."
  (c-set-offset 'innamespace [0])
  (pew-lsp/c-setup))

;;==============================================================================
;; Setup
;;==============================================================================

(add-hook 'c-mode-hook #'pew-lsp/c-setup)
(add-hook 'c++-mode-hook #'pew-lsp/cpp-setup)

(provide 'pack-lsp-c)
;;; pack-lsp-c.el ends here
