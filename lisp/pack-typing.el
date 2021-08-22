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
         ("<tab>" . company-complete-common-or-cycle)
         ("<RET>" . company-abort)
         ("<return" . company-abort))
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 10
        company-tooltip-idle-delay 0.2
        company-idle-delay 0.2
        company-show-numbers t
        company-minimum-prefix-length 1
        company-selection-wrap-around t)
  (global-company-mode 1)
  :config
  (company-tng-mode 1))

;;==============================================================================
;; Snippets
;;==============================================================================

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1))

(provide 'pack-typing)
;;; pack-typing.el ends here
