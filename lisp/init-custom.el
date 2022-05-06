;;; init-custom.el --- Vanilla Emacs configurations -*- lexical-binding: t -*-
;;; Commentary:

;; Vanilla Emacs configuration.
;; Avoid using `use-package' because it's supposed to be applied on any Emacs setup.

;;; Code:
;;;; Keybindings

(pew/set-key

 ("C-x t SPC" . tab-bar-select-tab-by-name)
 ("C-x t f" . tab-bar-switch-to-next-tab)
 ("C-x t b" . tab-bar-switch-to-prev-tab)
 ("C-x t m" . pew/move-tab-next)
 ("C-x t M" . pew/move-tab-prev)
 ("C-x t t" . tab-bar-new-tab)
 ("C-x t T" . pew/pop-window-in-new-tab)
 ("C-x t l" . tab-switcher)
 ("C-x C-d" . pew/open-cwd)
 ([remap next-buffer] . pew/next-buffer)
 ([remap previous-buffer] . pew/prev-buffer)

 )

;;;; Custom settingis

;; IMPORTANT NOTE:
;; Most of vanilla options are defined with `defcustom', which means if they are
;; set directly by `setq' they might NOT work as expected. However, if we use
;; `custom-set-variables' they would work but `custom-file' would produce a bunch
;; of duplicated settings. To address this issue, we can use
;; `customize-set-variable'. It calls those options' setters if they have and
;; also prevents writting settings from this file to `custom-file'.

;;;;; Startup

(pew/set-custom

 inhibit-startup-buffer-menu t
 inhibit-startup-echo-area-message t
 inhibit-startup-screen t
 inhibit-startup-message t
 initial-scratch-message ""

;;;;; Windows and frame

 display-buffer-base-action '((display-buffer-reuse-window display-buffer-below-selected)
                              (inhibit-switch-frame . t)
                              (reusable-frame))
 split-height-threshold 0
 split-width-threshold nil
 frame-resize-pixelwise t
 window-resize-pixelwise t
 help-window-select t
 
 ;; Scrolling
 scroll-conservatively 101
 scroll-step 2
 scroll-margin 2
 scroll-preserve-screen-position t
 hscroll-margin 2 ;; Avoid ending character overlapping in terminal mode
 hscroll-step 2
 auto-hscroll-mode t ;; set to 'current-line to scroll the current line only
 auto-window-vscroll t
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount '(2 ((shift) . 0.5) ((control) . text-scale))
 redisplay-skip-fontification-on-input t

;;;;; Interface elements

 ;; Cursor
 cursor-type 'box
 blink-cursor-mode nil
 mouse-yank-at-point t
 
 ;; Status
 column-number-indicator-zero-based nil
 column-number-mode t
 line-number-mode t
 size-indication-mode t
 ring-bell-function 'ignore

 ;; Line numbers
 display-line-numbers 'relative
 display-line-numbers-type 'relative
 global-display-line-numbers-mode t
 global-hl-line-mode t

 ;; Minibuffers
 enable-recursive-minibuffers t
 minibuffer-depth-indicate-mode t

 ;; Tabbar
 tab-bar-close-button-show nil
 tab-bar-new-button-show nil
 tab-bar-new-tab-choice nil
 tab-bar-position t
 tab-bar-select-tab-modifiers '(meta)
 tab-bar-tab-hints t
 tab-bar-tab-name-function 'tab-bar-tab-name-all
 tab-bar-show 1
 tab-bar-mode nil ;; Setting to t would cause display issue in terminal mode

;;;;; Formatting

;; Indentation
 tab-width 4
 indent-tabs-mode nil
 backward-delete-char-untabify-method 'hungry

;; Whitespaces
;; Leaving '(face ...) would cause confusion with `show-trailing-whitespace'
 whitespace-style '(trailing space-before-tab tab-mark)
 show-trailing-whitespace nil
 global-whitespace-mode t

 ;; Don't wrap by default
 truncate-lines t
 truncate-partial-width-windows nil
 
 ;; Don't skip any characters
 line-move-ignore-invisible nil
 line-move-visual nil
 
 ;; Column and fill
 fill-column 80
 adaptive-fill-mode nil
 display-fill-column-indicator t
 display-fill-column-indicator-column t
 global-display-fill-column-indicator-mode t

 ;; Coding system
 current-language-environment "UTF-8"
 default-input-method "chinese-py"
 inhibit-eol-conversion t
 require-final-newline t
 display-raw-bytes-as-hex t
 
 ;; Pairs
 show-paren-when-point-in-periphery t
 show-paren-when-point-inside-paren t
 show-paren-mode t

;;;;; QoL

 ;; Sanity
 use-short-answers t
 xterm-mouse-mode t
 shell-command-prompt-show-cwd t
 what-cursor-show-names t

 ;; Operations
 save-place-mode t
 delete-selection-mode t
 
 ;; Don't write files automatically
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 
 ;; TODO: newcomment
 
 ;; Recentf
 recentf-max-saved-items 250
 recentf-auto-cleanup 'never
 recentf-mode t
 
 ;; Clipboard
 select-enable-clipboard t
 select-enable-primary t
 
 ;; Repeat mode
 repeat-mode t
 repeat-exit-key (kbd "RET")
 repeat-exit-timeout 2

 ;; Let Emacs use minibuffer to prompt passphrase
 epg-pinentry-mode 'loopback

 )

;;;; Face settings

(pew/set-face

 tab-bar (:inherit 'default)

 )

;;;; Enable commands that are disabled by default

(pew/set-enabled
 
 scroll-left
 list-threads
 list-timers

 )

(provide 'init-custom)
;;; init-custom.el ends here
