;;; elpa-ui.el --- Theme collection -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is a simple collection of theme related stuff.
;; If the theme needs to be configured, move it to an individual module.

;;; Code:
;;; Modeline
;; Doom Emacs style modeline
(use-package doom-modeline
  :demand t

  :config
  (pewcfg
    :setq
    (doom-modeline-height 1)
    (doom-modeline-modal t)
    (doom-modeline-modal-icon nil)
    (doom-modeline-unicode-fallback nil)
    :switch
    (doom-modeline-unicode-fallback)
    :eval
    (doom-modeline-mode 1)))

;;; Lazy loadeding
(pew::use-package-later
;;;; Colors schemes
  doom-themes
  spacemacs-theme
  dracula-theme
  moe-theme
  ;; Fantastic themes from https://protesilaos.com/
  modus-themes
  ef-themes
  ;; New love
  catppuccin-theme
  monokai-theme

;;;; Icons
  all-the-icons)

;;; Default looking
(pew::load-theme 'catppuccin)

(provide 'elpa-ui)
;;; elpa-ui.el ends here
