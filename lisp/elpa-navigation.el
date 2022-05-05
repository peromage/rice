;;; elpa-navigation.el --- UI navigation -*- lexical-binding: t -*-
;;; Commentary:

;; This module configures packages that improve navigation experience.

;;; Code:

;;;; Completion framework
;;;;; Ivy

;; Contains ivy, counsel and swipper
;; See: https://oremacs.com/swiper/#installing-from-emacs-package-manager
(use-package counsel
  :demand t
  :diminish (ivy-mode counsel-mode)
  :bind (("C-s" . swiper)
         ("C-x d" . counsel-dired)
         ("C-x f" . counsel-find-file)
         ("C-c r" . ivy-resume)
         ("C-c f" . counsel-git)
         ("C-c h" . counsel-git-grep)
         ("C-c g" . counsel-ag)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :custom
  (ivy-use-virtual-buffers t)
  ;; enable this if you want `swiper' to use it
  ;(search-default-mode #'char-fold-to-regexp)
  (ivy-count-format "(%d/%d) ")
  (ivy-on-del-error-function #'ignore)
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  ;; Force minimal number of chars required for all searches
  (ivy-more-chars-alist '((t . 2)))
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (mapcar (lambda (name) (add-to-list 'ivy-ignore-buffers name t)) pew/special-buffers))

;; Make Ivy show more information
(use-package ivy-rich
  :requires ivy
  :custom
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1)
  (ivy-rich-set-columns
   'ivy-switch-buffer
   '((ivy-rich-switch-buffer-indicators
      (:width 4 :face error :align left))
     (ivy-switch-buffer-transformer
      (:width 0.35))
     (ivy-rich-switch-buffer-major-mode
      (:width 16 :face warning :align left))
     (ivy-rich-switch-buffer-size
      (:width 7 :align right))
     (ivy-rich-switch-buffer-project
      (:width 0.18 :face success :align right))
     (ivy-rich-switch-buffer-path
      (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path
                           x
                           (ivy-rich-minibuffer-width 0.3))) :align left)))))

;; Help sort candidates and also keep the most recent history on the top
(use-package ivy-prescient
  :requires ivy
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;;;; Prompt

;; Which-key is very informative to show keybindings when you forget them.
(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-popup-type 'side-window)
  (which-key-show-early-on-C-h nil)
  (which-key-idle-delay 1.0)
  :config
  (which-key-mode 1)
  ;; Minibuffer usually causes display problems
  ;(which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom))

;; Ido can be replaced by which-key and completion framework
(use-package ido
  :disabled
  :ensure nil
  :custom
  (ido-enable-flex-matching t)
  (ido-default-file-method 'selected-window)
  (ido-default-buffer-method 'selected-window)
  :config
  (ido-mode 1))

;;;; Workspace management

;; Edwina splits windows automatically in DWM fashions.
(use-package edwina
  :disabled
  ;; Enabled on demand
  :commands (edwina-mode edwin-mode)
  :config
  (edwina-setup-dwm-keys))

;; Like `winner-mode', `eyebrowse-mode' arranges multiple workspaces.
;; This functionality can be replaced with builtin tab which works pretty much the same.
(use-package eyebrowse
  :disabled
  :commands eyebrowse-mode)

;; Treemacs provides a sidebar for navigation.
(use-package treemacs
  :disabled)

;; Builtin window manager.
(use-package winner
  :disabled
  :ensure nil
  :config
  (winner-mode 1))

;; A pretty decent buffer manager to replace the default shabby one.
(use-package ibuffer
  :ensure nil
  :commands (ibuffer list-buffers)
  :init
  (defalias 'list-buffers 'ibuffer))

;;;; Project management

;; Projectile provides a convenient way navigate between different projects.
(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-enable-caching t)
  :config
  (projectile-mode 1))

;; Ivy integration
(use-package counsel-projectile
  :requires ivy
  :custom
  (projectile-completion-system 'ivy)
  :config
  (counsel-projectile-mode 1))

;;;; File management

;;;;; Dired helper functions

(defun pew/dired/go-to ()
  "Go into the current directory/file under the cursor without creating a new buffer."
  (interactive)
  (dired-find-alternate-file))

(defun pew/dired/go-up ()
  "Go to the parent directory without creating a new buffer."
  (interactive)
  (dired-up-directory)
  (dired-find-file)
  (find-alternate-file ".."))

(defun pew/dired/close-others ()
  "Close other Dired buffers but this one."
  (interactive)
  (pew/close-other-buffers-with-major-mode 'dired-mode))

;;;;; Dired

(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-find-file)
  :bind (:map dired-mode-map
         ("RET" . pew/dired/go-to) ;; was dired-find-file
         ("DEL" . pew/dired/go-up)) ;; was dired-unmark-backward
  :custom
  (dired-listing-switches "-alFD --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :ensure nil
  :after dired)

(provide 'elpa-navigation)
;;; elpa-navigation.el ends here
