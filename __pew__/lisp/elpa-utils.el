;;; elpa-utils.el --- supplementary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loading for these packages

;; Search
(use-package rg :ensure t :defer t)
;; Focused view
(use-package olivetti :ensure t :defer t)
;; Cats!!!
(use-package nyan-mode :ensure t :defer t)
(use-package zone-nyan :ensure t :defer t)
;; Colorful parenthesises
(use-package rainbow-delimiters :ensure t :defer t)
;; Colorize color code
(use-package rainbow-mode :ensure t :defer t)
;; Highlight current line
(use-package beacon :ensure t :defer t)
;; Hyperbole
(use-package hyperbole :ensure t :defer t)

;;; Editing

;; Plan B.  In case `flymake' doesn't have checkers for certain languages
(use-package flycheck
  :ensure t
  :commands (global-flycheck-mode flycheck-mode))

;; TODO: Maybe use tempel?
(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs (list (plist-get pew::path-plist :yas-template)))
  (yas-indent-line 'fixed)
  :config
  (yas-global-mode 1))

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
          ("f"   . avy-goto-char)
          ("M-f" . avy-goto-line)) )

(use-package ace-window
  :ensure t
  :bind ( :map pew::M-u-map
          ("w"   . ace-window)
          ("M-w" . ace-swap-window)) )

;;; Git

(use-package magit
  :ensure t
  :commands magit-status
  :bind ( :map pew::M-u-map
          ("g"   . magit-status)
          ("M-g" . magit-file-dispatch) )
  :custom
  ;; Don't use the default bindings under "C-x" prefix
  (magit-define-global-key-bindings nil))

(use-package git-gutter
  :ensure t
  :custom
  (git-gutter:modified-sign "**")
  (git-gutter:added-sign "++")
  (git-gutter:deleted-sign "--")
  (git-gutter:unchanged-sign nil)
  (git-gutter:separator-sign nil)
  (git-gutter:update-interval 2)
  (git-gutter:visual-line nil)
  (git-gutter:hide-gutter nil)
  (git-gutter:verbosity 0)

  :config
  (global-git-gutter-mode 1)

  (pewcfg :face
          (git-gutter:modified   :foreground  "yellow"       :background  "unspecified")
          (git-gutter:added      :foreground  "green"        :background  "unspecified")
          (git-gutter:deleted    :foreground  "red"          :background  "unspecified")
          (git-gutter:unchanged  :foreground  "unspecified"  :background  "unspecified")
          (git-gutter:separator  :foreground  "unspecified"  :background  "unspecified")))

;;; Panels

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

;;; Interactive

(use-package keycast
  :ensure t
  :commands (keycast-log-mode
             keycast-tab-bar-mode
             keycast-header-line-mode
             keycast-mode-line-mode)
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
                                          (mouse-set-point nil)
                                          (mouse-drag-region nil)
                                          (mwheel-scroll nil)
                                          (handle-select-window nil))
                                        keycast-substitute-alist)))

(use-package which-key
  :ensure t
  :commands which-key-mode
  :custom
  (which-key-popup-type 'side-window)
  (which-key-show-early-on-C-h nil)
  (which-key-idle-delay 1.0)
  :config
  (which-key-mode 1)
  ;; Minibuffer usually causes display problems
  ;;(which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom))

(provide 'elpa-utils)
;;; elpa-utils.el ends here
