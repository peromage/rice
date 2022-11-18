;;; elpa-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Utilities that provide convenience and enhance experience

;;; Code:

;;; Terminal
;; Vterm is a decent terminal emulator inside of Emacs.
;; NOTE: Not available on Windows.
(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :commands (vterm vterm-other-window)
  :hook (vterm-mode . pew/terminal-common-setup)

  :init
  (defun pew/vterm/new ()
    "Create a new vterm window with a unique name."
    (interactive)
    (with-current-buffer (vterm)
      (rename-buffer "*pewterm*" t)))

  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-tramp-shells '(("ssh" "/bin/bash")
                        ("plink" "/bin/bash"))))

;;; Tree navigation
(use-package treemacs
  :commands treemacs
  :hook (treemacs-mode . pew/treemacs/setup)

  :custom
  (treemacs-wrap-around nil)

  :config
  (defun pew/treemacs/setup ()
    "Treemacs mode setup."
    (setq-local display-line-numbers nil)))

(provide 'elpa-utils)
;;; elpa-utils.el ends here
