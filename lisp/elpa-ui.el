;;; elpa-ui.el --- Theme collection -*- lexical-binding: t -*-
;;; Commentary:

;; This file is a simple collection of theme related stuff.
;; If the theme needs to be configured, move it to an individual module.

;;; Code:

;;;; Colors schemes

(use-package doom-themes :defer t)
(use-package dracula-theme :defer t)
(use-package moe-theme :defer t)
(use-package modus-themes :defer t)

;;;; Icons

(use-package all-the-icons :defer t)

;;;; Modeline

;; Modeline taken from Doom Emacs.
(use-package doom-modeline
  ;; https://github.com/seagle0128/doom-modeline/issues/187#issuecomment-508973014
  :custom-face
  (mode-line ((t (:height 0.9))))
  (mode-line-inactive ((t (:height 0.9))))
  :custom
  (doom-modeline-height 1)
  :config
  (doom-modeline-mode 1))

;;;; Initial setup

(pew/load-theme 'doom-dracula)

(provide 'elpa-ui)
;;; elpa-ui.el ends here
