;;; elpa-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Utilities that provide convenience and enhance experience

;;; Code:

;;; Search
(use-package rg :defer t)

;;; Terminal
;; Vterm is a decent terminal emulator inside of Emacs.
;; NOTE: Not available on Windows.
(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :init
  (defun pew/vterm/new ()
    "Create a new vterm window with a unique name."
    (interactive)
    (with-current-buffer (vterm)
      (rename-buffer "*pewterm*" t)))

  :hook (vterm-mode . pew/terminal-common-setup)
  :commands (vterm vterm-other-window)
  :bind (:map vterm-mode-map
         ("ESC" . vterm-send-escape))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-tramp-shells '(("ssh" "/bin/bash")
                        ("plink" "/bin/bash"))))

(provide 'elpa-utils)
;;; elpa-utils.el ends here
