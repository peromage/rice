;;; pack-typing.el --- Coding convenience -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; Syntax checker
;;==============================================================================

(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode 1))

;;==============================================================================
;; Code completion
;;==============================================================================

(use-package company
  :diminish company-mode
  :bind (("C-c i" . company-complete)
         :map company-active-map
         ("<tab>" . company-complete)
         ("C-SPC" . company-search-abort))
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 10
        company-tooltip-idle-delay 0.1
        company-idle-delay 0.1
        company-show-numbers t
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-auto-complete nil)
  (global-company-mode 1)
  :config
  (company-tng-mode -1))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;==============================================================================
;; Snippets
;;==============================================================================

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1))

(provide 'pack-typing)
;;; pack-typing.el ends here
