;;; elpa-coding.el --- Packages for coding tasks -*- lexical-binding: t -*-
;;; Commentary:

;; This module configures for common coding needs including syntax checker, completion and language support.

;;; Code:

;;;; Completion
;;;;; Company

;; This configures `company-mode' and enhances its experience.
(use-package company
  :demand t
  :diminish company-mode
  :bind (:map company-mode-map
         ("C-c i" . company-complete)
         ([remap completion-at-point] . company-complete)
         :map company-active-map
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

;;;; Syntax and spell checker

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode 1))

;;;; Snippets

;; Default snippet directory is located at "snippets" in this PEW configuration.
(use-package yasnippet
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (expand-file-name "yasnippets" pew/home-dir)))
  :config
  (yas-global-mode 1))

(provide 'elpa-coding)
;;; elpa-coding.el ends here
