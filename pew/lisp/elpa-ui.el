;;; elpa-ui.el --- ui elements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: doom-modeline
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

;;; Lazy loadeding for these packages
(pew::use-package-later
  ;; Colors schemes
  doom-themes
  spacemacs-theme
  dracula-theme
  moe-theme
  catppuccin-theme
  monokai-theme
  ;; From https://protesilaos.com/
  modus-themes
  ef-themes
  ;; Icons
  all-the-icons)

;;; Default looking
(pew::load-theme 'catppuccin)

(provide 'elpa-ui)
;;; elpa-ui.el ends here
