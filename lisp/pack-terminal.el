;;; pack-terminal.el --- Terminal supports -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew-pack/term-setup ()
  "Setup for terminal on entering."
  (setq word-wrap nil
        truncate-lines nil
        truncate-partial-width-windows nil)
  (hl-line-mode -1)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

;; Eshell
(add-hook 'eshell-mode-hook #'pew-pack/term-setup)

;; Libvterm
(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :hook (vterm-mode . pew-pack/term-setup)
  :init
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 99999))

(use-package multi-vterm
  :after vterm)

(provide 'pack-terminal)
;;; pack-terminal.el ends here
