;;; elpa-lang-c.el --- c/c++ mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: cc-mode
(use-package cc-mode
  :hook ((c-mode . pew::cc-mode::c-oninit)
         (c++-mode . pew::cc-mode::c++-oninit))

  :config
  ;; Setup functions
  (defun pew::cc-mode::common-setup ()
    "Common CC mode setup."
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'innamespace 0)
    ;; Indentation
    (setq-local indent-tabs-mode nil)
    (setq-local c++-tab-always-indent t)
    (setq-local c-basic-offset 4)
    (setq-local c-indent-level 4)
    (setq-local tab-width 4)
    (setq-local tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
    (setq-local c-syntactic-indentation t)
    (setq-local c-syntactic-indentation-in-macros t)
    ;; Fill columns
    (setq-local adaptive-fill-mode nil)
    ;; Macro line continuation
    (setq-local c-backslash-column 80)
    (setq-local c-backslash-max-column 160)
    (setq-local c-auto-align-backslashes t))

  (defun pew::cc-mode::lsp-setup ()
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

  (defun pew::cc-mode::c-oninit ()
    "Initialization for C mode."
    (pew::cc-mode::common-setup)
    (pew::cc-mode::lsp-setup))

  (defun pew::cc-mode::c++-oninit ()
    "Initialization for C++ mode."
    (pew::cc-mode::common-setup)
    (pew::cc-mode::lsp-setup)))

(provide 'elpa-lang-c)
;;; elpa-lang-c.el ends here
