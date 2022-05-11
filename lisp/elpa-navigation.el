;;; elpa-navigation.el --- UI navigation -*- lexical-binding: t -*-
;;; Commentary:

;; This module configures packages that improve navigation experience.

;;; Code:

;;;; Prompt

;; Which-key is very informative to show keybindings when you forget them.
(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-popup-type 'side-window)
  (which-key-show-early-on-C-h nil)
  (which-key-idle-delay 1.0)
  :config
  (which-key-mode 1)
  ;; Minibuffer usually causes display problems
  ;(which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom))

;;;; Project management

;; Projectile provides a convenient way navigate between different projects.
(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-enable-caching t)
  :config
  (projectile-mode 1))

(use-package rg
  :defer t)

(provide 'elpa-navigation)
;;; elpa-navigation.el ends here
