;;; elpa-utils.el --- supplementary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: vterm
(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :commands (vterm vterm-other-window)
  :hook (vterm-mode . pewlib::as-terminal-mode)

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

;;; Package: treemacs
(use-package treemacs
  :commands treemacs
  :hook (treemacs-mode . pew::treemacs::on-enter)
  :bind ( :map treemacs-mode-map
          ("j" . treemacs-find-file) )

  :custom
  (treemacs-wrap-around nil)
  (treemacs-eldoc-display 'detailed)
  (treemacs-show-hidden-files t)
  (treemacs-hide-dot-git-directory nil)

  :config
  (defun pew::treemacs::on-enter ()
    "`treemacs-mode' initialization."
    (display-line-numbers-mode -1))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode nil)

  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

;;; Package: separedit
(use-package separedit
  :bind ( :map pew::M-u-map
          ("'" . separedit-dwim)) )

;;; Package: paredit -- Simplify S-expression editing
(use-package paredit
  :hook ((lisp-interaction-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-data-mode . paredit-mode)))

;;; Package: avy -- Jump among texts
(use-package avy
  :bind ( :map pew::M-u-map
          ("f" . avy-goto-char)
          ("j" . avy-goto-line)) )

;;; Package: ace-window -- Jump between windows
(use-package ace-window
  :bind ( :map pew::M-u-map
          ("w" . ace-window)
          ("W" . ace-swap-window)) )

;;; Lazy loadeding for these packages
(pewcfg::use-package-defer-list
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
  which-key
  ;; Hyperbole
  hyperbole)

(provide 'elpa-utils)
;;; elpa-utils.el ends here
