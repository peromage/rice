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
  (eglot-confirm-server-initiated-edits 'confirm))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p 1) ;; Single line so that minibuffer is not bouncing
  (eldoc-echo-area-prefer-doc-buffer nil) ;; Always display in minibuffer
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-idle-delay 0.2))

(provide 'elpa-eglot)
;;; elpa-eglot.el ends here
