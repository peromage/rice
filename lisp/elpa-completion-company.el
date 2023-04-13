;;; elpa-completion-company.el --- Completion by company -*- lexical-binding: t; -*-

;;; Commentary:
;; Use company as typing completion frontend and backend

;;; Code:
(use-package company
  :demand t

  :bind (:map
         company-mode-map
         ([remap completion-at-point] . company-complete)
         :map
         company-active-map
         ("TAB" . company-complete-common-or-cycle)
         ("C-c" . company-complete-selection)
         ("C-k" . company-abort))

  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10)
  (company-tooltip-idle-delay 0.2)
  (company-idle-delay 0.0)
  (company-show-numbers t)
  (company-show-quick-access t)
  (company-minimum-prefix-length 3)
  (company-selection-wrap-around t)
  (company-auto-complete nil)
  (company-abort-on-unique-match t)
  (company-require-match nil)
  (company-search-filtering t)

  :config
  (global-company-mode 1)
  (company-tng-mode 1))

(provide 'elpa-completion-company)
;;; elpa-completion-company.el ends here
