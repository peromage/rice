;;; elpa-utils.el --- Utilities -*- lexical-binding: t -*-
;;; Commentary:

;; Utilities that provide convenience and enhance experience

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

;;;; Search

(use-package rg
  :defer t)

;;;; Terminal

;; Vterm is a decent terminal emulator inside of Emacs.
;; NOTE: Not available on Windows.
(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :init
  (defun pew/vterm/new ()
    "Create a new vterm window with a unique name."
    (interactive)
    (vterm)
    (set-buffer "*vterm*")
    (rename-buffer "vterm" t))

  :hook (vterm-mode . pew/term-setup)
  :commands (vterm vterm-other-window)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000))

(provide 'elpa-utils)
;;; elpa-utils.el ends here
