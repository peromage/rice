;;; elpa-shell.el --- Terminal in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; Vterm is a decent terminal emulator inside of Emacs.
;; NOTE: Not available on Windows.

;;; Code:
;;;; Helper functions

(defun pew/shell/common-setup ()
  "Setup for terminal on entering."
  (setq-local word-wrap nil
              truncate-lines nil
              truncate-partial-width-windows nil
              global-hl-line-mode nil)
  (hl-line-mode -1)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

;;;; Libvterm

(defun pew/vterm/new ()
  "Create a new vterm window with a unique name."
  (interactive)
  (vterm)
  (set-buffer "*vterm*")
  (rename-buffer "vterm" t))

(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :hook (vterm-mode . pew/shell/common-setup)
  :commands (vterm vterm-other-window)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000))

;;;; Eshell

(use-package eshell
  :ensure nil
  :hook (eshell-mode . pew/shell/common-setup))

(provide 'elpa-shell)
;;; elpa-shell.el ends here
