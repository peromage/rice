;;; init-basic.el --- Basic Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Text format
(setq-default
 indent-tabs-mode nil
 tab-width 4
 fill-column 80
 comment-column 80
 buffer-file-coding-system 'utf-8-unix)
;;---
(set-language-environment "UTF-8")

;; Appearance
(setq-default
 ring-bell-function 'ignore
 inhibit-startup-screen t
 display-line-numbers-type 'relative
 cursor-type 'bar
 whitespace-style '(face trailing tab-mark)
 ;; Do not wrap by default
 word-wrap nil
 line-move-visual nil
 truncate-lines t
 truncate-partial-width-windows nil)
;;---
(load-theme 'tango-dark t nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)
(size-indication-mode 1)
(show-paren-mode 1)
(global-display-line-numbers-mode 1)
(visual-line-mode -1)
(global-whitespace-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode -1)

;; Quality of life
(setq-default
 major-mode 'text-mode
 ;; No backup and auto-save
 make-backup-files nil
 auto-save-timeout 0
 auto-save-interval 0
 ;; No bookmark file
 bookmark-file nil
 ;; Lock files are needed
 create-lockfiles t
 ;; Smooth scrolling
 mouse-wheel-scroll-amount '(1 ((shift) . 5))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse t
 scroll-step 1)
;;---
(electric-pair-mode 1)

(provide 'init-basic)
;;; init-basic.el ends here
