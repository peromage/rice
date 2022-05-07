;;; elpa-shell.el --- Terminal in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; Vterm is a decent terminal emulator inside of Emacs.
;; NOTE: Not available on Windows.

;;; Code:
;;;; Libvterm

(defun pew/vterm/new ()
  "Create a new vterm window with a unique name."
  (interactive)
  (vterm)
  (set-buffer "*vterm*")
  (rename-buffer "vterm" t))

(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :hook (vterm-mode . pew/term-setup)
  :commands (vterm vterm-other-window)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000))

(provide 'elpa-shell)
;;; elpa-shell.el ends here
