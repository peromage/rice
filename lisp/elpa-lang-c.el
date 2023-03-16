;;; elpa-lang-c.el --- C/C++ mode -*- lexical-binding: t; -*-

;;; Commentary:
;; C/C++ major mode configuration.

;;; Code:
(use-package cc-mode
  :hook ((c-mode . pew/cc-mode/c-setup)
         (c++-mode . pew/cc-mode/c++-setup))

  :config
  ;; Setup functions
  (defun pew/cc-mode/common-setup ()
    "Common CC mode setup."
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'innamespace 0)
    (setq-local c++-tab-always-indent t)
    (setq-local c-basic-offset 4)
                                        ;(setq-local c-syntactic-indentation nil)
                                        ;(setq-local c-syntactic-indentation-in-macros nil)
    (setq-local c-indent-level 4)
    (setq-local tab-width 4)
    (setq-local tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
    (setq-local indent-tabs-mode nil)
    (setq-local adaptive-fill-mode nil))

  (defun pew/cc-mode/lsp-setup ()
    "LSP setup for CC mode."
    ;; Prevent Clangd from inserting headers itself
    (setq lsp-clients-clangd-args '("-j=8"
                                    "--background-index"
                                    "--clang-tidy"
                                    "--completion-style=detailed"
                                    "--pch-storage=disk"
                                    "--header-insertion=never"
                                    "--header-insertion-decorators=0"
                                    "--suggest-missing-includes"
                                    "--all-scopes-completion"))
    (lsp-deferred)
    ;; Post LSP mode settings
    (electric-indent-mode -1))

  (defun pew/cc-mode/c-setup ()
    "Initialization for C mode."
    (pew/cc-mode/common-setup)
    (pew/cc-mode/lsp-setup))

  (defun pew/cc-mode/c++-setup ()
    "Initialization for C++ mode."
    (pew/cc-mode/common-setup)
    (pew/cc-mode/lsp-setup)))

(provide 'elpa-lang-c)
;;; elpa-lang-c.el ends here
