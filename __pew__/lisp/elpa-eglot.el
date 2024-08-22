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
  (eglot-autoshutdown t) ;; Save resources
  (eglot-extend-to-xref nil) ;; Don't include files outside of current project
  (eglot-report-progress t))

(provide 'elpa-eglot)
;;; elpa-eglot.el ends here
