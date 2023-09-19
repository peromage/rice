;;; elpa-lang-c.el --- c/c++ mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: cc-mode
(pewcfg::use-package cc-mode
  :defer t
  :hook ((c-mode . pew::cc-mode::c-oninit)
         (c++-mode . pew::cc-mode::c++-oninit))

  :config
  ;; Setup functions
  (defun pew::cc-mode::common-setup ()
    "Common CC mode setup."
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'innamespace 0)
    ;; Indentation
    (setq-local indent-tabs-mode nil
                c++-tab-always-indent t
                c-basic-offset 4
                c-indent-level 4
                tab-width 4
                tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
                c-syntactic-indentation t
                c-syntactic-indentation-in-macros t
                ;; Fill columns
                adaptive-fill-mode nil
                ;; Macro line continuation
                c-backslash-column 80
                c-backslash-max-column 160
                c-auto-align-backslashes t))

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
