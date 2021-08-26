;;; pack-terminal.el --- Terminal supports -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :init
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 99999)
  :config
  (use-package multi-vterm))

(provide 'pack-terminal)
;;; pack-terminal.el ends here
