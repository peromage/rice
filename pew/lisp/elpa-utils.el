;;; elpa-utils.el --- Utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities that provide convenience and enhance experience

;;; Code:
;;; Terminal
;; Vterm is a decent terminal emulator inside of Emacs.
;; NOTE: Not available on Windows.
(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :commands (vterm vterm-other-window)
  :hook (vterm-mode . pew::terminal-mode-on-init)

  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-keymap-exceptions '("C-z" "C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x"
                             "M-o" "C-y" "M-y"))
  (vterm-tramp-shells '(("ssh" "/bin/bash")
                        ("plink" "/bin/bash")))

  :init
  (defun pew::vterm::new (arg)
    "Create a new vterm window.
ARG is a prefix argument.  If it is non-nill, a prompt will pop up to allow
users to specify the shell to start with."
    (interactive "P")
    (if arg
        (let ((vterm-shell (read-string "Shell: ")))
          (vterm :new))
      (vterm :new))))

;;; Tree navigation
(use-package treemacs
  :commands treemacs
  :hook (treemacs-mode . pew::treemacs::on-init)

  :custom
  (treemacs-wrap-around nil)

  :config
  (defun pew::treemacs::on-init ()
    "`treemacs-mode' initialization."
    (display-line-numbers-mode -1)))

;;; Separate edit
(use-package separedit
  :bind (:map pew::M-u-map
         ("'" . separedit-dwim)))

;;; Simplify S-expression editing
(use-package paredit
  :hook ((lisp-interaction-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-data-mode . paredit-mode)))

;;; Jump among texts
(use-package avy
  :bind (:map pew::M-u-map
         ("f" . avy-goto-char)
         ("j" . avy-goto-line)))

;;; Jump between windows
(use-package ace-window
  :bind (:map pew::M-u-map
         ("w" . ace-window)
         ("W" . ace-swap-window)))

;;; Lazy loadeding
(pew::use-package-later
  ;; Search
  rg

  ;; Focused view
  olivetti

  ;; Nyanyanya!!!
  nyan-mode
  zone-nyan

  ;; Colorful parenthesises
  rainbow-delimiters

  ;; Colorize color code
  rainbow-mode

  ;; Highlight current line
  beacon

  ;; Sometimes useful to get prompted for LSP commands
  which-key)

(provide 'elpa-utils)
;;; elpa-utils.el ends here