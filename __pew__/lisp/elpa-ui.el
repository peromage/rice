;;; elpa-ui.el --- ui elements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loadeding for these packages
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

;;; Modelines

(use-package doom-modeline
  :disabled
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

(use-package prot-modeline
  :ensure nil ;; site-lisp
  :demand t)

;;; Misc

(use-package spacious-padding
  :demand t
  :custom
  (spacious-padding-subtle-mode-line t)
  :config
  (spacious-padding-mode 1))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package kind-icon
  :disabled
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Setup functions for convenience
(defun pew::install-fonts ()
  (interactive)
  (all-the-icons-install-fonts :silent)
  (nerd-icons-install-fonts :silent))

;; Default looking
(pewlib::debug::load-theme 'dracula)

(provide 'elpa-ui)
;;; elpa-ui.el ends here
