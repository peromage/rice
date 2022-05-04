;;; elpa-edwina.el --- DWM like window management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package edwina
  ;; Enabled on demand
  :commands (edwina-mode edwin-mode)
  :config
  (edwina-setup-dwm-keys))

(provide 'elpa-edwina)
;;; elpa-edwina.el ends here
