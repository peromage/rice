;;; pack-terminal.el --- Terminal supports -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; Functions and variables
;;==============================================================================

(defun pew-term/setup ()
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
(add-hook 'eshell-mode-hook #'pew-term/setup)

;; Libvterm
(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :hook (vterm-mode . pew-term/setup)
  :init
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 99999))

(use-package multi-vterm
  :requires vterm)

(provide 'pack-terminal)
;;; pack-terminal.el ends here
