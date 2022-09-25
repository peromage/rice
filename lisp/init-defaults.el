;;; init-defaults.el --- Vanilla Emacs configurations -*- lexical-binding: t -*-
;;; Commentary:
;; Vanilla Emacs configuration.
;; Avoid using `use-package' because it's supposed to be applied on any Emacs setup.

;;; Code:
;; NOTE: Most of vanilla options are defined with `defcustom', which means if they
;; are set directly by `setq' they might NOT work as expected.  However, if we use
;; `custom-set-variables' they would work but `custom-file' would produce a bunch
;; of duplicated settings.  To address this issue, we can use
;; `customize-set-variable'.  It calls those options' setters if they have and
;; also prevents writting settings from this file to `custom-file'.

(pew/config
;;; Custom
  :custom
;;;; Startup
  (inhibit-startup-buffer-menu t)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (initial-scratch-message "")

;;;; Windows and frames
  ;; Over-3-side-window causes troubles when toggling (I don't know why)
  (display-buffer-alist `((,(pew/special-buffer (shell term help) t)
                           (display-buffer-reuse-window display-buffer-in-side-window)
                           (window-height . 0.25)
                           (side . bottom)
                           (slot . 0))
                          (,(pew/special-buffer (message backtrace warning log compilation output command) t)
                           (display-buffer-reuse-window display-buffer-in-side-window)
                           (window-height . 0.25)
                           (side . bottom)
                           (slot . 1))))

  ;; See `split-window-sensibly' and `window-splittable-p'
  (split-height-threshold 20 "10 lines minimal")
  (split-width-threshold 160 "80 columns minimal")
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (help-window-select t)

  ;; Scrolling
  (scroll-conservatively 101)
  (scroll-step 1)
  (scroll-margin 2)
  (scroll-preserve-screen-position t)
  (hscroll-margin 3 "Avoid ending character overlapping in terminal mode")
  (hscroll-step 1)
  (auto-hscroll-mode t "set to 'current-line to scroll the current line only")
  (auto-window-vscroll t)
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(2 ((shift) . 0.5) ((control) . text-scale)))
  (redisplay-skip-fontification-on-input t)
  ;; Don't skip any characters
  (line-move-ignore-invisible nil)
  (line-move-visual nil)

;;;; Interface elements
  ;; Cursor
  (cursor-type 'box)
  (blink-cursor-mode nil)
  (mouse-yank-at-point t)

  ;; Misc
  (ring-bell-function 'ignore)

  ;; Modeline
  (column-number-indicator-zero-based nil "Required by `doom-modeline'")
  (column-number-mode t)
  (line-number-mode t)
  (size-indication-mode t)
  (mode-line-compact 'long)
  (mode-line-position-column-format '(" C%C") "One-based column number")
  (mode-line-position-line-format '(" L%l"))
  (mode-line-position-column-line-format '(" %l:%C"))
  (mode-line-percent-position '(-3 "%o"))
  (mode-line-defining-kbd-macro '(:propertize " Macro" face mode-line-emphasis))
  ;; Simplify mode display
  (mode-line-modes (mapcar (lambda (x)
                             (cond
                              ;; Minimize minor mode indicator
                              ((and (listp x)
                                    (listp (cadr x))
                                    (eq 'minor-mode-alist (cadr (cadr x))))
                               '((defining-kbd-macro mode-line-defining-kbd-macro)))
                              ;; Remove parens around mode indicator
                              ((and (stringp x)
                                    (string-match-p "^[()]$" x))
                               nil)
                              (t x)))
                           mode-line-modes))

  ;; Window
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t "Right and bottom")
  (window-divider-mode nil "Disabled by default")

  ;; Line numbers
  (display-line-numbers 'relative)
  (display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)
  (global-hl-line-mode t)

  ;; Minibuffers
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Tabbar
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice nil "Duplicate current tab")
  (tab-bar-position t)
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-function #'tab-bar-tab-name-truncated)
  (tab-bar-show 1)
  (tab-bar-mode nil "Setting to t would cause display issue in terminal mode")

;;;; Formatting
  ;; Indentation
  (tab-width 4)
  (indent-tabs-mode nil)
  (backward-delete-char-untabify-method 'hungry)

  ;; Whitespaces
  ;; Leaving '(face ...) would cause confusion with `show-trailing-whitespace'
  (whitespace-style '(face trailing space-before-tab missing-newline-at-eof tab-mark))
  (show-trailing-whitespace t)
  (global-whitespace-mode t)

  ;; Don't wrap by default
  (truncate-lines t)
  (truncate-partial-width-windows nil)

  ;; Column and fill
  (fill-column 80)
  (adaptive-fill-mode nil)
  (display-fill-column-indicator t)
  (display-fill-column-indicator-column t)
  (global-display-fill-column-indicator-mode t)

  ;; Encoding and locale
  (coding-system-for-write 'utf-8-unix)
  (buffer-file-coding-system 'utf-8-unix)
  (current-language-environment "UTF-8")
  (default-input-method "chinese-py")
  (inhibit-eol-conversion t)
  (require-final-newline t)
  (display-raw-bytes-as-hex t)

;;;; QoL
  ;; Sanity
  (use-short-answers t)
  (xterm-mouse-mode t)
  (context-menu-mode t)
  (shell-command-prompt-show-cwd t)
  (what-cursor-show-names t)
  ;; Log warnings in the background instead of poping the window up
  (native-comp-async-report-warnings-errors 'silent)

  ;; Operations
  (save-place-mode t)
  (delete-selection-mode t)
  (delete-by-moving-to-trash nil)

  ;; Don't write files automatically
  (auto-save-default nil)
  (create-lockfiles nil)
  (make-backup-files nil)

  ;; Auto refresh buffer
  (global-auto-revert-mode t)

  ;; Clipboard
  (select-enable-clipboard t)
  (select-enable-primary t)

  ;; Let Emacs use minibuffer to prompt passphrase
  (epg-pinentry-mode 'loopback)

;;;; Some useful builtin packages
  ;; TODO: newcomment
  ;; Pairs
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren t)
  (show-paren-mode t)

  ;; Recentf
  (recentf-max-saved-items 250)
  (recentf-auto-cleanup 'never)
  (recentf-mode t)

  ;; Save history
  (savehist-save-minibuffer-history t)
  (savehist-mode t)

  ;; Repeat mode
  (repeat-mode nil)
  (repeat-exit-key (kbd "C-g"))
  (repeat-exit-timeout 2)

  ;; Dired
  (dired-listing-switches "-alFD --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer nil "Cannot open multiple dired windows if on")

  ;; ibuffer
  (ibuffer-movement-cycle nil)
  ;; Check `ibuffer-filtering-alist' for quilifiers.
  (ibuffer-saved-filter-groups `(("PEW"
                                  ("Doc" (or (mode . org-mode)
                                             (mode . markdown-mode)))
                                  ("Dired" (mode . dired-mode))
                                  ("Shell" (or (mode . shell-mode)
                                               (mode . eshell-mode)
                                               (mode . term-mode)
                                               (mode . vterm-mode)))
                                  ("Git" (name . ,(pew/special-buffer magit)))
                                  ("VC" (name . ,(pew/special-buffer vc)))
                                  ("Ediff" (name . ,(pew/special-buffer ediff)))
                                  ;; Putting to last to avoid buffers being wrongly categorized as "special"
                                  ("Special" (starred-name)))))

  ;; isearch and search
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (search-highlight t)
  (search-highlight-submatches t)
  (query-replace-highlight t)
  (query-replace-lazy-highlight t)
  (query-replace-highlight-submatches t)
  (query-replace-show-replacement t)

  ;; ispell
  (ispell-dictionary "en_US")

  ;; ediff
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-vertically)

  ;; electric
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-mode nil "Annoying sometimes")
  (electric-indent-mode nil "Annoying sometimes")

  ;; TRAMP
  (tramp-default-method "ssh")

  ;; Winner mode
  (winner-mode t)
  (winner-dont-bind-my-keys t)

;;; Switch commands
  :switch
  (indent-tabs-mode)
  (show-trailing-whitespace)
  (line-move-visual)
  (debug-on-error)
  (display-line-numbers '(nil absolute relative visual))

;;; Transient keybindings
  :transient
  (pewkey
   ;; Windows
   ("q" . pew/close-window)
   ("o" . pew/next-window)
   ("O" . pew/prev-window)
   ("<left>" . shrink-window-horizontally)
   ("<down>" . shrink-window)
   ("<up>" . enlarge-window)
   ("<right>" . enlarge-window-horizontally)
   ("s" . split-window-below)
   ("S" . window-toggle-side-windows)
   ("v" . split-window-right)
   ;; Layout
   ("y" . winner-undo)
   ("Y" . winner-redo)
   ;; Other window
   ("M-f" . pew/scroll-other-window-page-down)
   ("M-b" . pew/scroll-other-window-page-up)
   ("M-e" . pew/scroll-other-window-line-down)
   ("M-y" . pew/scroll-other-window-line-up)
   ("M-l" . pew/recenter-other-window)

   ;; Buffers
   ("r" . rename-buffer)
   ("w" . save-buffer)
   ("n" . pew/next-buffer)
   ("p" . pew/prev-buffer)
   ("g" . pew/buffer-full-path)
   ("B" . display-buffer)

   ;; Tabs
   ("R" . tab-bar-rename-tab)
   ("f" . tab-bar-switch-to-next-tab)
   ("F" . tab-bar-select-tab-by-name)
   ("b" . tab-bar-switch-to-prev-tab)
   ("t" . tab-bar-new-tab)
   ("T" . pew/pop-window-in-new-tab)
   ("m" . pew/move-tab-next)
   ("M" . pew/move-tab-prev)
   ("Q" . tab-bar-close-tab)

   ;; Edit
   ("u" . undo)
   ("U" . undo-redo)
   (";" . comment-line)
   ("c" . isearch-forward-regexp)
   ("C" . isearch-query-replace-regexp)

   ;; Org
   ("i" . org-capture)
   ("I" . org-agenda)

   ;; Zoom (zooming in/out depends on the last key.  see `text-scale-adjust')
   ("+" . text-scale-adjust)
   ("-" . text-scale-adjust)
   ("0" . text-scale-adjust)

   ;; Frame Transparency
   ("]" . pew/increase-frame-opacity)
   ("[" . pew/decrease-frame-opacity))

;;; Mode keybindings
  :bind
;;;; Global
  (global
   ;; File and directory browsing
   ("C-x C-d" . dired-jump)

   ;; Xref
   ("C-x j" . xref-find-definitions)
   ("C-x J" . xref-find-references)
   ("C-x C-j" . xref-find-apropos)

   ;; Remap for better experience
   ([remap next-buffer] . pew/next-buffer)
   ([remap previous-buffer] . pew/prev-buffer)
   ([remap list-buffers] . ibuffer)
   ([remap isearch-delete-char] . isearch-del-char)

   ;; Pewkey
   ("C-x g" . pewkey)
   ("C-x C-g" . pewkey-repeat))

;;;; Dired
  (dired-mode
   ("RET" . pew/dired-go-to)
   ("DEL" . pew/dired-go-up)
   ("f" . dired-find-file)
   ("b" . dired-up-directory)
   ("<left>" . dired-up-directory)
   ("<right>" . dired-find-file))

;;; Mode hooks
  :hook
  ;; Make shell clean
  (eshell-mode . pew/term-setup)
  (shell-mode . pew/term-setup)

  ;; Don't save trailing spaces
  (after-save . pew/delete-trailing-whitespaces)

  ;; Don't move cursor to the minibuffer prompt
  (minibuffer-setup . cursor-intangible-mode)

  ;; Don't spawn new windows
  (grep-mode . pew/reuse-window-setup)

  ;; Enable folding
  (prog-mode . outline-minor-mode)

;;; Symbol properties
  :property
  ;; Enable commands that are disabled by default
  (scroll-left disabled nil)
  (list-threads disabled nil)
  (list-timers disabled nil)
  (dired-find-alternate-file disabled nil)
  (upcase-region disabled nil)
  (downcase-region disabled nil)

;;; Face settings
  :face
  (default :family "Iosevka" :foundry "UKWN" :slant normal :weight normal :height 120 :width normal)
  (tab-bar :inherit default))

(provide 'init-defaults)
;;; init-defaults.el ends here
