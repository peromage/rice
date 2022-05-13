;;; init-defaults.el --- Vanilla Emacs configurations -*- lexical-binding: t -*-
;;; Commentary:

;; Vanilla Emacs configuration.
;; Avoid using `use-package' because it's supposed to be applied on any Emacs setup.

;;; Code:
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

 display-buffer-base-action '((display-buffer-reuse-window
                               display-buffer-in-previous-window
                               display-buffer-below-selected))
 ;; See `window-splittable-p'
 split-height-threshold 0
 split-width-threshold nil
 frame-resize-pixelwise t
 window-resize-pixelwise t
 help-window-select t

 ;; Scrolling
 scroll-conservatively 101
 scroll-step 1
 scroll-margin 2
 scroll-preserve-screen-position t
 hscroll-margin 3 ;; Avoid ending character overlapping in terminal mode
 hscroll-step 1
 auto-hscroll-mode t ;; set to 'current-line to scroll the current line only
 auto-window-vscroll t
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount '(2 ((shift) . 0.5) ((control) . text-scale))
 redisplay-skip-fontification-on-input t
 ;; Don't skip any characters
 line-move-ignore-invisible nil
 line-move-visual nil

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
 minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)

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
 whitespace-style '(face trailing space-before-tab missing-newline-at-eof tab-mark)
 show-trailing-whitespace t
 global-whitespace-mode t

 ;; Don't wrap by default
 truncate-lines t
 truncate-partial-width-windows nil

 ;; Column and fill
 fill-column 80
 adaptive-fill-mode nil
 display-fill-column-indicator t
 display-fill-column-indicator-column t
 global-display-fill-column-indicator-mode t

 ;; Locale
 current-language-environment "UTF-8"
 default-input-method "chinese-py"
 inhibit-eol-conversion t
 require-final-newline t
 display-raw-bytes-as-hex t

;;;;; QoL

 ;; Sanity
 use-short-answers t
 xterm-mouse-mode t
 shell-command-prompt-show-cwd t
 what-cursor-show-names t
 ;; Log warnings in the background instead of poping the window up
 native-comp-async-report-warnings-errors 'silent

 ;; Operations
 save-place-mode t
 delete-selection-mode t

 ;; Don't write files automatically
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil

 ;; Auto refresh buffer
 global-auto-revert-mode t

 ;; Clipboard
 select-enable-clipboard t
 select-enable-primary t

 ;; Let Emacs use minibuffer to prompt passphrase
 epg-pinentry-mode 'loopback

;;;;; Some useful builtin packages

 ;; TODO: newcomment

 ;; Pairs
 show-paren-when-point-in-periphery t
 show-paren-when-point-inside-paren t
 show-paren-mode t

 ;; Recentf
 recentf-max-saved-items 250
 recentf-auto-cleanup 'never
 recentf-mode t

 ;; Save history
 savehist-save-minibuffer-history t
 savehist-mode t

 ;; Repeat mode
 repeat-mode t
 repeat-exit-key (kbd "RET")
 repeat-exit-timeout 2

 ;; Dired
 dired-listing-switches "-alFD --group-directories-first"
 dired-dwim-target t
 dired-recursive-copies 'always
 dired-recursive-deletes 'always
 dired-kill-when-opening-new-dired-buffer nil ;; Cannot open multiple dired windows if on

 ;; ibuffer
 ibuffer-movement-cycle nil
 ;; Check `ibuffer-filtering-alist' for quilifiers.
 ibuffer-saved-filter-groups '(("PEW"
                                ("Dired" (mode . dired-mode))
                                ("Shell" (or (mode . shell-mode)
                                             (mode . eshell-mode)
                                             (mode . term-mode)
                                             (mode . vterm-mode)))
                                ("Git" (name . "magit"))
                                ;; Putting to last to avoid buffers being wrongly categorized as "special"
                                ("Special" (starred-name))))

 ;; isearch
 isearch-lazy-count t

 ;; ispell
 ispell-dictionary "en_US"

 ;; ediff
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-vertically

 ;; electric
 electric-pair-preserve-balance t
 electric-pair-delete-adjacent-pairs t
 electric-pair-mode nil ;; Annoying sometimes
 electric-indent-mode nil ;; Same above

 )

;;;; Keybindings

(pew/set-key global-map

 ;; Tab bindings
 "C-x t SPC" #'tab-bar-select-tab-by-name
 "C-x t f" #'tab-bar-switch-to-next-tab
 "C-x t b" #'tab-bar-switch-to-prev-tab
 "C-x t m" #'pew/move-tab-next
 "C-x t M" #'pew/move-tab-prev
 "C-x t t" #'tab-bar-new-tab
 "C-x t T" #'pew/pop-window-in-new-tab
 "C-x t l" #'tab-switcher

 ;; File and directory browsing
 "C-x f" #'find-file
 "C-x C-f" #'pew/open-cwd

 ;; Remap for better experience
 [remap next-buffer] #'pew/next-buffer
 [remap previous-buffer] #'pew/prev-buffer
 [remap list-buffers] #'ibuffer
 [remap isearch-delete-char] #'isearch-del-char

 )

(pew/set-key dired-mode-map

 "RET" #'pew/dired-go-to
 "DEL" #'pew/dired-go-up

 )

;;;; Mode hooks

(pew/set-hook

 'eshell-mode-hook #'pew/term-setup
 'after-save-hook #'pew/delete-trailing-whitespaces

 ;; Don't move cursor to the minibuffer prompt
 'minibuffer-setup-hook #'cursor-intangible-mode

 )

;;;; Enable commands that are disabled by default

(pew/set-enabled

 'scroll-left
 'list-threads
 'list-timers
 'dired-find-alternate-file

 )

;;;; Face settings

(pew/set-face

 'tab-bar '(:inherit default)

 )

(provide 'init-defaults)
;;; init-defaults.el ends here
