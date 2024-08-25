;;; elpa-eglot.el --- Language server by Eglot -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :bind ( :map eglot-mode-map
          ("C-c l f" . eglot-format)
          ("C-c l F" . eglot-format-buffer)
          ("C-c l r" . eglot-rename)
          ("C-c l a" . eglot-code-actions)
          ("C-c l q" . eglot-code-action-quickfix)
          ("C-c l i" . imenu) )
  :custom
  (eglot-strict-mode nil)
  (eglot-autoshutdown nil) ;; Reverting buffer causes auto shutdown so turn it off
  (eglot-extend-to-xref nil) ;; Don't include files outside of current project
  (eglot-report-progress t)
  (eglot-confirm-server-initiated-edits 'confirm)

  :config
  (setq eglot-server-programs (nconc '((((nix-mode :language-id "nix")
                                         (nix-ts-mode :language-id "nix"))
                                        . ("nixd")))
                                     eglot-server-programs)))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p 1) ;; Single line so that minibuffer is not bouncing
  (eldoc-echo-area-prefer-doc-buffer nil) ;; Always display in minibuffer
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-idle-delay 0.2)
  ;; Note: The order of the displayed message in echo area is determined by the
  ;; order of functions in `eldoc-documentation-functions'.
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(provide 'elpa-eglot)
;;; elpa-eglot.el ends here
