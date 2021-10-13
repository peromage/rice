;;; pack-coding-lsp-c.el --- C/C++ mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Functions and variables
;;------------------------------------------------------------------------------

(defun pew/c-mode/setup ()
  "Initialization for C mode."
  (c-set-offset 'inextern-lang 0)
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

(defun pew/cpp-mode/setup ()
  "Initialization for C++ mode."
  (c-set-offset 'innamespace 0)
  (pew/c-mode/setup))

;;------------------------------------------------------------------------------
;; Setup
;;------------------------------------------------------------------------------

(add-hook 'c-mode-hook #'pew/c-mode/setup)
(add-hook 'c++-mode-hook #'pew/cpp-mode/setup)

(provide 'pack-coding-lsp-c)
;;; pack-coding-lsp-c.el ends here