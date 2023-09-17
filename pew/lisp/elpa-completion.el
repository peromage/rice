;;; elpa-completion.el --- supplementary completion packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: flymake-shellcheck -- Syntax and spell checker
;; TODO: Remove this in Emacs 29.
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (sh-mode . flymake-shellcheck-load))

;;; Package: flycheck
;; Plan B.  In case `flymake' doesn't have checkers for certain languages
(use-package flycheck
  :defer t
  :commands (global-flycheck-mode flycheck-mode))

;;; Package: yasnippet -- Easy snippets
(use-package yasnippet
  :custom
  (yas-snippet-dirs (list pew::yasnippet-template-dir))

  :config
  (yas-global-mode 1))

;; TODO: tempel

(provide 'elpa-completion)
;;; elpa-completion.el ends here
