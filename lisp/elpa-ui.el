;;; elpa-ui.el --- Theme collection -*- lexical-binding: t -*-
;;; Commentary:
;; This file is a simple collection of theme related stuff.
;; If the theme needs to be configured, move it to an individual module.

;;; Code:
;;; Colors schemes
(use-package doom-themes :defer t)
(use-package dracula-theme :defer t)
(use-package moe-theme :defer t)
(use-package modus-themes :defer t)
(use-package spacemacs-theme :defer t)

;;; Icons
(use-package all-the-icons :defer t)

;;; Modeline
;; Modeline taken from Doom Emacs.
(use-package doom-modeline
  :custom
  (doom-modeline-height 1)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-modal-icon t)
  :config
  (doom-modeline-mode 1))

;;; Default looking
(pew/load-theme 'doom-dracula)

(provide 'elpa-ui)
;;; elpa-ui.el ends here
