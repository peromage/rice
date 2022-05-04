;;; elpa-edwina.el --- Window split control -*- lexical-binding: t -*-
;;; Commentary:

;; Edwina splits windows automatically in DWM fashions.

;;; Code:

(use-package edwina
  ;; Enabled on demand
  :commands (edwina-mode edwin-mode)
  :config
  (edwina-setup-dwm-keys))

(provide 'elpa-edwina)
;;; elpa-edwina.el ends here
