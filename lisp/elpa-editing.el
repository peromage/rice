;;; elpa-editing.el --- Editing support -*- lexical-binding: t -*-

;;; Commentary:
;; Editing related configurations including completion, syntax checker, search
;; and snippet etc.

;;; Code:

;;; Company for completion
;; This configures `company-mode' and enhances its experience.
(use-package company
  :demand t

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

;;; Syntax and spell checker
(use-package flycheck
  :config
  (global-flycheck-mode 1))

;;; Snippets
;; Default snippet directory is located at "snippets" in this PEW configuration.
(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (expand-file-name "yasnippets" pew/home-dir)))

  :config
  (yas-global-mode 1))

;;; Search
(use-package rg
  :defer t)

;;; Separate edit
(use-package separedit
  :defer t)

;;; Simplify S-expression editing
(use-package paredit
  :defer t)

;;; Jump around
(use-package avy
  :defer t)

(provide 'elpa-editing)
;;; elpa-editing.el ends here
