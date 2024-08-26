;;; elpa-ui.el --- ui elements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-height 1)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon nil)
  (doom-modeline-unicode-fallback nil)

  :config
  (doom-modeline-mode 1)
  (pewcfg :switch
          (doom-modeline-unicode-fallback)))

;; Lazy loadeding for these packages
(pewcfg::use-package-defer-list
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
  ;; Fonts and icons
  all-the-icons
  nerd-icons)

;; Setup functions for convenience
(defun pew::install-fonts ()
  (interactive)
  (all-the-icons-install-fonts :silent)
  (nerd-icons-install-fonts :silent))

;; Default looking
(pewlib::debug::load-theme 'dracula)

(provide 'elpa-ui)
;;; elpa-ui.el ends here
