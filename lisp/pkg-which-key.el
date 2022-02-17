;;; pkg-which-key.el --- Keybinding hints -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-popup-type 'minibuffer
        which-key-show-early-on-C-h nil
        which-key-idle-delay 1.0)
  (which-key-mode 1)
  (which-key-setup-minibuffer))

(provide 'pkg-which-key)
;;; pkg-which-key.el ends here
