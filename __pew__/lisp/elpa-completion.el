;;; elpa-completion.el --- supplementary completion packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: flymake-shellcheck -- Syntax and spell checker
;; TODO: Remove this in Emacs 29.
(pewcfg::use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (sh-mode . flymake-shellcheck-load))

;;; Package: flycheck
;; Plan B.  In case `flymake' doesn't have checkers for certain languages
(pewcfg::use-package flycheck
  :defer t
  :commands (global-flycheck-mode flycheck-mode))

;;; Package: yasnippet -- Easy snippets
(pewcfg::use-package yasnippet
  :custom
  (yas-snippet-dirs (list (plist-get pew::paths-plist :yas-template)))
  (yas-indent-line 'fixed)

  :config
  (yas-global-mode 1))

;; TODO: tempel

(provide 'elpa-completion)
;;; elpa-completion.el ends here
