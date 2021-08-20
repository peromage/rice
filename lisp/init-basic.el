;;; init-basic.el --- Basic Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; =============================================================================
;; Settings by variables
;; =============================================================================

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

;; =============================================================================
;; Settings by functions
;; =============================================================================

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

(provide 'init-basic)
;;; init-basic.el ends here
