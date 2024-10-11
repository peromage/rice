;;; elpa-eglot.el --- Language server by Eglot -*- lexical-binding: t; -*-
;;; Commentary:

;; Notes for most used LSP servers:
;; C++: clangd (clang-tools)
;; Python: pipx install python-lsp-server[all]

;;; Code:

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook (eglot-managed-mode . pew::eglot::on-enter-managed-buffer)
  :bind ( :map pew::M-u-map
          ("M-l" . eglot)
          :map eglot-mode-map
          ("C-c l f" . eglot-format)
          ("C-c l F" . eglot-format-buffer)
          ("C-c l r" . eglot-rename)
          ("C-c l a" . eglot-code-actions)
          ("C-c l q" . eglot-code-action-quickfix)
          ("C-c l l" . imenu) )
  :custom
  (eglot-strict-mode nil)
  (eglot-autoshutdown nil) ;; Reverting buffer causes auto shutdown so turn it off
  (eglot-extend-to-xref nil) ;; Don't include files outside of current project
  (eglot-report-progress t)
  (eglot-confirm-server-initiated-edits 'confirm)

  :preface
  (defun pew::eglot::on-enter-managed-buffer ()
    (eglot-inlay-hints-mode -1))

  :config
  (setq eglot-server-programs (nconc '(;; Nix
                                       (((nix-mode :language-id "nix")
                                         (nix-ts-mode :language-id "nix"))
                                        . ("nixd"))
                                       ;; C/C++
                                       ((c-mode c-ts-mode c++-mode c++-ts-mode)
                                        . ("clangd"
                                           ;; Performance
                                           "-j 8"
                                           "--background-index"
                                           "--background-index-priority=normal"
                                           "--pch-storage=memory"
                                           ;; Formatting
                                           "--fallback-style=Google"
                                           ;; Completion
                                           "--clang-tidy"
                                           "--all-scopes-completion"
                                           "--completion-style=detailed"
                                           "--function-arg-placeholders"
                                           "--header-insertion=never"
                                           "--header-insertion-decorators")))
                                     ;; Default builtins
                                     eglot-server-programs)))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p 1) ;; Single line so that minibuffer is not bouncing
  (eldoc-echo-area-prefer-doc-buffer nil) ;; Always display in minibuffer
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-idle-delay 0.2)
  ;; NOTE: The order of the displayed message in echo area is determined by the
  ;; order of functions in `eldoc-documentation-functions'.
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(provide 'elpa-eglot)
;;; elpa-eglot.el ends here
