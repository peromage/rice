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
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;==============================================================================
;; Settings by variables
;;==============================================================================

(setq-default

 ;; Startup
 inhibit-startup-screen t
 inhibit-startup-buffer-menu t
 inhibit-startup-echo-area-message t

 ;; Appearance
 display-line-numbers-type 'relative
 cursor-type 'bar
 whitespace-style '(face trailing tab-mark)

 ;; Do not wrap by default
 word-wrap nil
 line-move-visual nil
 truncate-lines t
 truncate-partial-width-windows nil

 ;; Text format
 indent-tabs-mode nil
 tab-width 4
 fill-column 80
 comment-column 80
 buffer-file-coding-system 'utf-8-unix

 ;; No annoying bell
 ring-bell-function 'ignore
 visible-bell nil

 ;; Default major mode
 major-mode 'text-mode

 ;; No backup and auto-save
 make-backup-files nil
 auto-save-timeout 0
 auto-save-interval 0

 ;; No bookmark file
 bookmark-file nil

 ;; Use lock files
 create-lockfiles t

 ;; Smooth scrolling
 mouse-wheel-scroll-amount '(2 ((shift) . 5))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse t
 scroll-step 1

 ;; Disable automatic horizontal splits
 split-width-threshold nil
 split-height-threshold 0)

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

;; Convenience
(electric-pair-mode 1)
(delete-selection-mode 1)

;; Default theme
(load-theme 'tango-dark t nil)

;; Encoding
(set-language-environment "UTF-8")

;;==============================================================================
;; TUI settings
;;==============================================================================

(unless window-system
  (xterm-mouse-mode 1))

(provide 'init-vanilla)
;;; init-vanilla.el ends here
