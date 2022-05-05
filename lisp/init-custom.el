;;; init-custom.el --- Vanilla Emacs configurations -*- lexical-binding: t -*-
;;; Commentary:

;; This file contains the settings for vanilla Emacs.

;;; Code:
;;;; Keybindings

(pew/global-set-key '(("C-x t SPC" . tab-bar-select-tab-by-name)
                      ("C-x t f" . tab-bar-switch-to-next-tab)
                      ("C-x t b" . tab-bar-switch-to-prev-tab)
                      ("C-x t m" . pew/move-tab-next)
                      ("C-x t M" . pew/move-tab-prev)
                      ("C-x t t" . tab-bar-new-tab)
                      ("C-x t T" . pew/pop-window-in-new-tab)
                      ("C-x t l" . tab-switcher)
                      ("C-x C-d" . pew/open-cwd)
                      ([remap next-buffer] . pew/next-buffer)
                      ([remap previous-buffer] . pew/prev-buffer)))

;;;; Custom settingis

(custom-set-variables
 ;; Startup
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-echo-area-message "")
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")

 ;; Interface
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(cursor-type 'box)
 '(size-indication-mode t)
 '(display-line-numbers 'relative)
 '(display-line-numbers-type 'relative)
 '(enable-recursive-minibuffers t)
 '(ring-bell-function 'ignore)
 '(global-display-line-numbers-mode t)
 '(global-hl-line-mode t)

 ;; Windows
 '(display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-below-selected)
     (inhibit-switch-frame . t)
     (reusable-frame)))
 '(split-height-threshold 0)
 '(split-width-threshold nil)
 '(frame-resize-pixelwise t)

 ;; Tabs and indentations
 '(tab-width 4)
 '(indent-tabs-mode nil)
 '(backward-delete-char-untabify-method 'hungry)

 ;; Whitespaces
 ;; Leaving '(face ...) would cause confusion with `show-trailing-whitespace'
 '(whitespace-style '(trailing space-before-tab tab-mark))
 '(show-trailing-whitespace nil)
 '(global-whitespace-mode t)

 ;; Columns and fill
 '(truncate-lines t)
 '(truncate-partial-width-windows nil)
 '(line-move-ignore-invisible nil)
 '(line-move-visual nil)
 '(adaptive-fill-mode nil)
 '(column-number-mode t)
 '(fill-column 80)
 '(display-fill-column-indicator t)
 '(display-fill-column-indicator-column t)
 '(global-display-fill-column-indicator-mode t)

 ;; Scrolling
 '(scroll-conservatively 101)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(2 ((shift) . 0.5) ((control) . text-scale)))
 '(hscroll-margin 2) ;; Settting to 2 to avoid ending character overlapping in terminal mode
 '(hscroll-step 1)

 ;; Tabbar
 '(tab-bar-close-button-show nil)
 '(tab-bar-new-button-show nil)
 '(tab-bar-new-tab-choice nil)
 '(tab-bar-position t)
 '(tab-bar-select-tab-modifiers '(meta))
 '(tab-bar-show 1)
 '(tab-bar-tab-hints t)
 '(tab-bar-mode nil) ;; Setting to t would cause display issue in terminal mode

 ;; Coding system
 '(current-language-environment "UTF-8")
 '(default-input-method "chinese-py")
 '(inhibit-eol-conversion t)
 '(require-final-newline t)

 ;; Accessibility
 '(show-paren-mode t)
 '(xterm-mouse-mode t)

 ;; Operations
 '(delete-selection-mode t)
 '(help-window-select t)
 '(auto-save-default nil)
 '(create-lockfiles nil)
 '(make-backup-files nil)
 '(recentf-mode t)
 )

;;;; Face settings

(custom-set-faces
 '(tab-bar ((t (:inherit default)))))

;;;; Enable commands that are disabled by default

(put 'scroll-left 'disabled nil)

(provide 'init-custom)
;;; init-custom.el ends here
