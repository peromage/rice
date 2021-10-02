;;; pack-terminal.el --- Terminal supports -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; Functions and variables
;;==============================================================================

(defun pew/theme/setup ()
  "Setup for terminal on entering."
  (setq-local word-wrap nil
              truncate-lines nil
              truncate-partial-width-windows nil
              global-hl-line-mode nil)
  (hl-line-mode -1)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

;;==============================================================================
;; Setup
;;==============================================================================

;; Eshell
(add-hook 'eshell-mode-hook #'pew/theme/setup)

;; Libvterm
(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :hook (vterm-mode . pew/theme/setup)
  :commands (vterm vterm-other-window)
  :init
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 9999))

(use-package multi-vterm
  :after vterm
  :commands (multi-vterm multi-vterm-project))

(provide 'pack-terminal)
;;; pack-terminal.el ends here
