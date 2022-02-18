;;; pkg-vterm.el --- Terminal in Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Libvterm
(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :hook (vterm-mode . pew/terminal-setup)
  :commands (vterm vterm-other-window)
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 9999))

(use-package multi-vterm
  :after vterm
  :commands (multi-vterm multi-vterm-project))

(provide 'pkg-vterm)
;;; pkg-vterm.el ends here
