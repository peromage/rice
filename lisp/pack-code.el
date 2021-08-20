;;; pack-code.el --- Coding convenience -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; Syntax checker
;;==============================================================================

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode 1))

;;==============================================================================
;; Code completion
;;==============================================================================

(use-package company
  :config
  (company-mode 1))

;;==============================================================================
;; Snippets
;;==============================================================================

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(provide 'pack-code)
;;; pack-code.el ends here
