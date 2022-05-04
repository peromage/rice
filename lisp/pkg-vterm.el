;;; pkg-vterm.el --- Terminal in Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Libvterm
(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :hook (vterm-mode . pew/terminal-setup)
  :commands (vterm vterm-other-window)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 9999))

;; Multiple vterm windows
(defun pew/vterm/new ()
  "Create a new vterm window with a unique name."
  (interactive)
  (vterm)
  (set-buffer "*vterm*")
  (rename-buffer "vterm" t))

(provide 'pkg-vterm)
;;; pkg-vterm.el ends here
