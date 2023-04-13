;;; elpa-completion.el --- Editing support -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion related packages including typing prompt, syntax checker, and
;; snippet etc..

;;; Code:
;;; Submodules
;; (require 'elpa-completion-ivy)
(require 'elpa-completion-vertico)
;; (require 'elpa-completion-company)
(require 'elpa-completion-corfu)

;;; Syntax and spell checker
(use-package flycheck
  :config
  (global-flycheck-mode 1))

;;; Snippets
;; Default snippet directory is located at "snippets" in this PEW configuration.
(use-package yasnippet
  :custom
  (yas-snippet-dirs (list pew::yasnippet-template-dir))

  :config
  (yas-global-mode 1))

(provide 'elpa-completion)
;;; elpa-completion.el ends here
