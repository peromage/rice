;;; elpa-ui.el --- Theme collection -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is a simple collection of theme related stuff.
;; If the theme needs to be configured, move it to an individual module.

;;; Code:
;;; Colors schemes
(use-package doom-themes :defer t)
(use-package spacemacs-theme :defer t)
(use-package dracula-theme :defer t)
(use-package moe-theme :defer t)
;; Fantastic themes from https://protesilaos.com/
(use-package modus-themes :defer t)
(use-package ef-themes :defer t)

;;; Icons
(use-package all-the-icons :defer t)

;;; Modeline
;; Modeline taken from Doom Emacs.
(use-package doom-modeline
  :custom
  (doom-modeline-height 1)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon t)

  :config
  (doom-modeline-mode 1)

  (pewcfg
    :switch
    (doom-modeline-unicode-fallback)))

;;; Default looking
(pew::load-theme 'modus-vivendi)

(provide 'elpa-ui)
;;; elpa-ui.el ends here
