;;; elpa-themes.el --- Theme collection -*- lexical-binding: t -*-
;;; Commentary:

;; This file is a simple collection of theme related stuff.
;; If the theme needs to be configured, move it to an individual module.

;;; Code:

;; Colors schemes
(use-package doom-themes :defer t)
(use-package dracula-theme :defer t)
(use-package moe-theme :defer t)
(use-package modus-themes :defer t)

;; Icons
(use-package all-the-icons :defer t)

;; Default theme
(pew/load-theme 'doom-dracula)

(provide 'elpa-themes)
;;; elpa-themes.el ends here
