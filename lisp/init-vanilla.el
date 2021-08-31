;;; init-vanilla.el --- Vanilla Emacs configurations -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This file should only contains the universal vanilla Emacs settings.
;;; This should be loaded first since it is the core configuration.
;;;
;;; Code:

;;==============================================================================
;; Initialization
;;==============================================================================

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Allow access from emacsclient
(defun pew/start-emacs-daemon ()
  "Start daemon if it does not exist."
  (require 'server)
  (unless (server-running-p)
    (message "[pew] Starting Emacs daemon...")
    (server-start)))
(add-hook 'after-init-hook #'pew/start-emacs-daemon)

;;==============================================================================
;; Settings by variables
;;==============================================================================

(setq-default

 ;; Startup
 inhibit-startup-screen t
 inhibit-startup-buffer-menu t
 ;; Does not work but I don't want a nasty hack. Set it in customize instead if
 ;; it is really bothering See:
 ;; https://yann.hodique.info/blog/rant-obfuscation-in-emacs/
 inhibit-startup-echo-area-message user-login-name

 ;; Appearance
 display-line-numbers-type 'relative
 cursor-type 'bar
 whitespace-style '(face trailing tab-mark)

 ;; No line wrap by default
 word-wrap t
 line-move-visual nil
 truncate-lines t
 truncate-partial-width-windows nil

 ;; Indentation
 indent-tabs-mode nil
 tab-width 4
 backward-delete-char-untabify-method 'hungry

 ;; Ruler
 fill-column 80
 comment-column 80
 display-fill-column-indicator-column 80

 ;; No annoying bell
 ring-bell-function 'ignore
 visible-bell nil

 ;; Default major mode
 major-mode 'text-mode

 ;; Smooth scrolling
 mouse-wheel-scroll-amount '(2 ((shift) . 5))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse t
 ;; See: https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html
 scroll-conservatively 101
 scroll-step 0
 scroll-up-aggressively 0.0
 scroll-down-aggressively 0.0
 scroll-margin 0
 scroll-preserve-screen-position t
 auto-window-vscroll nil

 ;; Window splitting behavior
 split-width-threshold nil
 split-height-threshold 0
 help-window-select t
 ;; Splits properly
 display-buffer-base-action
 '((display-buffer-reuse-window display-buffer-below-selected)
   (inhibit-switch-frame . t)
   (reusable-frames . nil))

 ;; Ido mode settings
 ido-enable-flex-matching t
 ido-default-file-method 'selected-window
 ido-default-buffer-method 'selected-window

 ;; Input method for emergency
 default-input-method "chinese-py"

 ;; Local files
 ido-save-directory-list-file (expand-file-name "cache/ido.last" user-emacs-directory)
 bookmark-default-file (expand-file-name "cache/bookmarks" user-emacs-directory)
 recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory)
 ;; No lock files
 create-lockfiles nil
 ;; No backup and auto-save
 make-backup-files nil
 auto-save-timeout 0
 auto-save-interval 0

 ;; Scratch buffer message
 initial-scratch-message ""

 ;; Better minibuffer
 enable-recursive-minibuffers t
 resize-mini-windows 'grow-only
 max-mini-window-height 0.3

 ;; Final newline
 require-final-newline t
 mode-require-final-newline t)

;;==============================================================================
;; Settings by functions
;;==============================================================================

;; Appearance
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)
(size-indication-mode 1)
(show-paren-mode 1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode -1)
(global-whitespace-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode -1)
(global-display-fill-column-indicator-mode 1)

;; Convenience
(auto-fill-mode -1)
(delete-selection-mode 1)
(winner-mode 1)
(recentf-mode 1)
(ido-mode 1)

;; Electric helpers
(electric-pair-mode 1)
(electric-indent-mode 1)

;; Default theme
(load-theme 'tango-dark t nil)

;; Encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

;; Replaces the default crap buffer manager with ibuffer
(defalias 'list-buffers 'ibuffer)

;;==============================================================================
;; TUI settings
;;==============================================================================

(unless window-system
  (xterm-mouse-mode 1))

(provide 'init-vanilla)
;;; init-vanilla.el ends here
