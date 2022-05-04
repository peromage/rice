;;; elpa-mode-cc.el --- C/C++ mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Setup functions
;;------------------------------------------------------------------------------

(defun pew/cc-mode/common-setup ()
  "Common CC mode setup."
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (setq c++-tab-always-indent t
        c-basic-offset 4
        ;c-syntactic-indentation nil
        ;c-syntactic-indentation-in-macros nil
        c-indent-level 4
        tab-width 4
        tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
        indent-tabs-mode nil
        adaptive-fill-mode nil))

(defun pew/cc-mode/c-setup ()
  "Initialization for C mode."
  (setq lsp-enable-on-type-formatting nil
        ;; Prevent Clangd from inserting headers itself
        lsp-clients-clangd-args '("-j=8"
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
  (electric-indent-mode 1))

(defun pew/cc-mode/cpp-setup ()
  "Initialization for C++ mode."
  (pew/cc-mode/c-setup))

;;------------------------------------------------------------------------------
;; Hooks
;;------------------------------------------------------------------------------

(add-hook 'c-mode-common-hook #'pew/cc-mode/common-setup)
(add-hook 'c-mode-hook #'pew/cc-mode/c-setup)
(add-hook 'c++-mode-hook #'pew/cc-mode/cpp-setup)

(provide 'elpa-mode-cc)
;;; elpa-mode-cc.el ends here
