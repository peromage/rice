;;; elpa-ui.el --- ui elements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loadeding for these packages

;; Colors schemes
(use-package doom-themes :ensure t :defer t)
(use-package spacemacs-theme :ensure t :defer t)
(use-package dracula-theme :ensure t :defer t)
(use-package moe-theme :ensure t :defer t)
(use-package catppuccin-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)
;; From https://protesilaos.com/
(use-package modus-themes :ensure t :defer t)
(use-package ef-themes :ensure t :defer t)
;; Fonts and icons
(use-package all-the-icons :ensure t :defer t)
(use-package nerd-icons :ensure t :defer t)

;;; Modelines

(use-package doom-modeline
  :disabled
  :ensure t
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
  :ensure t
  :demand t
  :custom
  (spacious-padding-subtle-mode-line t)
  :config
  (spacious-padding-mode 1))

(use-package nerd-icons-completion
  :ensure t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package kind-icon
  :disabled
  :ensure t
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
(pewlib::debug::load-theme 'modus-vivendi)

(provide 'elpa-ui)
;;; elpa-ui.el ends here
