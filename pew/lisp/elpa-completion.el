;;; elpa-completion.el --- Editing support -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion related packages including typing prompt, syntax checker, and
;; snippet etc..

;;; Code:
;;; Syntax and spell checker
;; TODO: Remove this in Emacs 29.
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (sh-mode . flymake-shellcheck-load))

;; Plan B.  In case `flymake' doesn't have checkers for certain languages
(use-package flycheck
  :commands (global-flycheck-mode flycheck-mode))

;;; Snippets
;; Default snippet directory is located at "snippets" in this PEW configuration.
(use-package yasnippet
  :config
  (pewcfg
    :setq
    (yas-snippet-dirs (list pew::yasnippet-template-dir))
    :eval
    (yas-global-mode 1)))

;;; TODO: tempel

(provide 'elpa-completion)
;;; elpa-completion.el ends here
