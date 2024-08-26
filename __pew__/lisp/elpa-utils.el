;;; elpa-utils.el --- supplementary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Lazy loadeding for these packages
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

;; Plan B.  In case `flymake' doesn't have checkers for certain languages
(use-package flycheck
  :ensure t
  :defer t
  :commands (global-flycheck-mode flycheck-mode))

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs (list (plist-get pew::path-plist :yas-template)))
  (yas-indent-line 'fixed)

  :config
  (yas-global-mode 1))

;; TODO: tempel

(use-package vterm
  :ensure t
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :commands (vterm vterm-other-window)
  :hook (vterm-mode . pewlib::editor::as-terminal-mode)

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

(use-package treemacs
  :ensure t
  :commands treemacs
  :hook (treemacs-mode . pew::treemacs::on-enter)
  :bind ( :map treemacs-mode-map
          ("j" . treemacs-find-file) )

  :custom
  (treemacs-wrap-around nil)
  (treemacs-eldoc-display 'detailed)
  (treemacs-show-hidden-files t)
  (treemacs-hide-dot-git-directory nil)

  :preface
  (defun pew::treemacs::on-enter ()
    "`treemacs-mode' initialization."
    (display-line-numbers-mode -1))

  :config
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

(use-package separedit
  :ensure t
  :bind ( :map pew::M-u-map
          ("'" . separedit-dwim)) )

(use-package paredit
  :ensure t
  :hook ((lisp-interaction-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-data-mode . paredit-mode)))

(use-package avy
  :ensure t
  :bind ( :map pew::M-u-map
          ("f" . avy-goto-char)
          ("j" . avy-goto-line)) )

(use-package ace-window
  :ensure t
  :bind ( :map pew::M-u-map
          ("w" . ace-window)
          ("W" . ace-swap-window)) )

(use-package keycast
  :ensure t
  :defer t
  :custom
  (keycast-mode-line-format "%2s%k%c%R")
  (keycast-mode-line-window-predicate 'mode-line-window-selected-p) ;; Show in current window
  (keycast-mode-line-remove-tail-elements nil)
  (keycast-tab-bar-format "%k%c%R")
  (keycast-header-line-format "%k%c%R")
  (keycast-header-line-remove-tail-elements nil)
  :config
  (setq keycast-substitute-alist (nconc '((self-insert-command "." "Typing...")
                                          (mouse-event-p nil)
                                          (mouse-movement-p nil)
                                          (mwheel-scroll nil))
                                   keycast-substitute-alist)))

(provide 'elpa-utils)
;;; elpa-utils.el ends here
