;;; init-vanilla.el --- Vanilla Emacs configurations -*- lexical-binding: t -*-
;;; Commentary:

;; This file should only contains the universal vanilla Emacs settings.
;; This should be loaded first since it is the core configuration.

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
(add-hook 'after-init-hook #'pew/start-emacs-daemon)

;;==============================================================================
;; Visual
;;==============================================================================

(setq-default
 ;; Startup
 inhibit-startup-screen t
 inhibit-startup-buffer-menu t
 ;; Does not work but I don't want a nasty hack. Set it in customize instead if
 ;; it is really bothering See:
 ;; https://yann.hodique.info/blog/rant-obfuscation-in-emacs/
 inhibit-startup-echo-area-message user-login-name
 ;; No annoying bell
 ring-bell-function 'ignore
 visible-bell nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)
(size-indication-mode 1)

;;==============================================================================
;; Editor
;;==============================================================================

(setq-default
 ;; Default major mode
 major-mode 'text-mode
 ;; Scratch buffer message
 initial-scratch-message ""
 ;; Whitespaces
 whitespace-style '(face trailing tab-mark)
 ;; Ruler
 fill-column 80
 comment-column 80
 adaptive-fill-mode nil
 display-fill-column-indicator-column 80
 ;; Line number
 display-line-numbers-type 'relative
 ;; Cursor
 cursor-type 'bar
 ;; No line wrap by default
 word-wrap t
 line-move-visual nil
 truncate-lines t
 truncate-partial-width-windows nil
 ;; Indentation
 indent-tabs-mode nil
 tab-width 4
 backward-delete-char-untabify-method 'hungry
 ;; Final newline
 require-final-newline t
 mode-require-final-newline t)

(global-whitespace-mode 1)
(global-display-fill-column-indicator-mode 1)
(global-visual-line-mode -1)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(auto-fill-mode -1)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode -1)
;; Make backspace and del keys behave as expected
;;(normal-erase-is-backspace-mode -1)

;;==============================================================================
;; Windows
;;==============================================================================

(setq-default
 ;; Window splitting behavior
 split-width-threshold nil
 split-height-threshold 0
 help-window-select t
 ;; Splits properly
 display-buffer-base-action '((display-buffer-reuse-window display-buffer-below-selected)
                              (inhibit-switch-frame . t)
                              (reusable-frames . nil))
 ;; Better minibuffer
 enable-recursive-minibuffers t
 resize-mini-windows 'grow-only
 max-mini-window-height 0.3
 ;; Better scrolling
 mouse-wheel-scroll-amount '(2 ((shift) . 5))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse t
 ;; Vertical scrolling
 ;; See: https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html
 scroll-conservatively 101
 scroll-step 0
 scroll-up-aggressively 0.0
 scroll-down-aggressively 0.0
 scroll-margin 0
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 ;; Horizontal scrolling
 hscroll-step 1
 hscroll-margin 0
 auto-hscroll-mode t)

(winner-mode 1)

;;==============================================================================
;; Tabbar
;;==============================================================================

(setq-default
 ;; Tab bar
 ;; Since Emacs 27 the built-in tab is pretty viable to be used as workspace
 ;; management like Eyebrowse.  This package is aimed to make it hadier.
 tab-bar-close-button-show nil
 tab-bar-close-last-tab-choice nil
 tab-bar-history-mode nil
 tab-bar-new-button-show nil
 tab-bar-new-tab-choice nil
 tab-bar-new-tab-to 'right
 tab-bar-position t
 tab-bar-select-tab-modifiers '(meta)
 tab-bar-show 1
 tab-bar-tab-hints t
 tab-bar-tab-name-function 'tab-bar-tab-name-current
 tool-bar-mode nil)

;;==============================================================================
;; Encoding
;;==============================================================================

(setq-default
 ;; Input method for emergency
 default-input-method "chinese-py")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

;;==============================================================================
;; Misc
;;==============================================================================

(setq-default
 ;; Local files
 bookmark-default-file (expand-file-name "cache/bookmarks" user-emacs-directory)
 recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory)
 ;; No lock files
 create-lockfiles nil
 ;; No backup and auto-save
 make-backup-files nil
 auto-save-timeout 0
 auto-save-interval 0)

(recentf-mode 1)
;; Replaces the default crap buffer manager with ibuffer
(defalias 'list-buffers 'ibuffer)


;;==============================================================================
;; Electric mode
;;==============================================================================

(setq-default
 ;; No closing pair if the open pair precedes a non-whitespace character
 electric-pair-preserve-balance nil
 electric-pair-delete-adjacent-pairs nil)

(electric-pair-mode 1)
(electric-indent-mode 1)

;;==============================================================================
;; Ido mode
;;==============================================================================

(setq-default
 ;; Ido mode settings
 ido-enable-flex-matching t
 ido-default-file-method 'selected-window
 ido-default-buffer-method 'selected-window
 ido-save-directory-list-file (expand-file-name "cache/ido.last" user-emacs-directory))

(ido-mode 1)

;;==============================================================================
;; Keybindings
;;==============================================================================

(let ((global-keys
       '(("C-x t SPC" . tab-bar-select-tab-by-name)
         ("C-x t f" . tab-bar-switch-to-next-tab)
         ("C-x t b" . tab-bar-switch-to-prev-tab)
         ("C-x t >" . (lambda () (interactive) (tab-bar-move-tab 1)))
         ("C-x t <" . (lambda () (interactive) (tab-bar-move-tab -1)))
         ("C-x t t" . tab-bar-new-tab)
         ("C-x t l" . tab-switcher)
         ([remap next-buffer] . pew/next-buffer)
         ([remap previous-buffer] . pew/prev-buffer))))
  (pew/global-set-key global-keys))

;;==============================================================================
;; GUI and TUI settings
;;==============================================================================

(cond ((window-system)
       (normal-erase-is-backspace-mode 1)
       (set-face-attribute 'tab-bar nil :inherit 'default))
      ;; In terminal
      (t
       ;; Enable C-h in terminal
       (normal-erase-is-backspace-mode -1)
       (xterm-mouse-mode 1)))

(provide 'init-vanilla)
;;; init-vanilla.el ends here
