;;; init-defaults.el --- built-in config -*- lexical-binding: t; -*-
;;; Commentary:

;; Set Emacs built-ins with my own flavor.

;;; Code:

(pewcfg
;;; Custom
  :customize
;;;; Startup
  (inhibit-startup-buffer-menu t)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (initial-scratch-message "")

;;;; Windows and frames
  ;; When 3 side windows present `window-toggle-side-windows' may cause problem
  ;; Use `winner-undo' to revert the mess
  (display-buffer-alist `((,(pew::special-buffer '(shell terminal))
                           ,@(pew::side-window-actions 'bottom 0))
                          (,(pew::special-buffer '(help))
                           ,@(pew::side-window-actions 'bottom 1))
                          (,(pew::special-buffer '(message backtrace warning log compilation output command))
                           ,@(pew::side-window-actions 'bottom 2))))

  ;; See `split-window-sensibly' and `window-splittable-p'
  (split-height-threshold 20 "10 lines minimal")
  (split-width-threshold 160 "80 columns minimal")
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (help-window-select nil "Always select the window")
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t "Right and bottom")
  (window-divider-mode nil "Disabled by default")

;;;; Scrolling
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

;;;; Modeline
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

;;;; Line numbers
  (display-line-numbers 'relative)
  (display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)
  (global-hl-line-mode t)

;;;; Minibuffers
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

;;;; Tabbar
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

;;;;; Cursor
  (cursor-type 'box)
  (blink-cursor-mode nil)
  (mouse-yank-at-point t)
  (delete-selection-mode t)

;;;; Tabs
  (indent-tabs-mode nil)
  (tab-width 4)
  (tab-always-indent t "Hybrid indentation and completion with `complete'")
  (backward-delete-char-untabify-method nil "Delete only one character at once")

;;;; Completion
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (bookmark-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-cycle-threshold nil "Always expand list")
  (completion-styles '(basic partial-completion))
  (completion-category-overrides '((file (styles basic partial-completion))))
  ;; (icomplete-vertical-mode t) ;; May conflict with other completion framework

;;;; Whitespaces
  ;; Leaving '(face ...) would cause confusion with `show-trailing-whitespace'
  (whitespace-style '(face trailing space-before-tab missing-newline-at-eof tab-mark))
  (show-trailing-whitespace t)
  (global-whitespace-mode t)
  (require-final-newline t)

;;;; Line fold
  ;; No wrapping
  (truncate-lines t)
  (truncate-partial-width-windows nil)
  ;; Fill columns
  (fill-column 80)
  (adaptive-fill-mode nil)
  (display-fill-column-indicator t)
  (display-fill-column-indicator-column t)
  (global-display-fill-column-indicator-mode t)

;;;; Encoding and locale
  (coding-system-for-write 'utf-8-unix)
  (buffer-file-coding-system 'utf-8-unix)
  (current-language-environment "UTF-8")
  (default-input-method "chinese-py")
  (inhibit-eol-conversion t)
  (display-raw-bytes-as-hex t)

;;;; Misc
  (use-short-answers t)
  (xterm-mouse-mode t)
  (context-menu-mode t)
  (shell-command-prompt-show-cwd t)
  (what-cursor-show-names t)
  (ring-bell-function 'ignore)
  (delete-by-moving-to-trash nil)
  ;; Log warnings in the background instead of poping the window up
  (native-comp-async-report-warnings-errors 'silent)
  ;; Pinentry -- Let Emacs use minibuffer to prompt passphrase
  (epg-pinentry-mode 'loopback)

;;;; File save
  (auto-save-default nil)
  (create-lockfiles nil)
  (make-backup-files nil)

;;;; Buffer auto refresh
  (global-auto-revert-mode t)

;;;; Clipboard
  (select-enable-clipboard t)
  (select-enable-primary t)

;;;; Pairs
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren t)
  (show-paren-mode t)

;;;; History persistence
  ;; Recentf mode
  (recentf-max-saved-items 250)
  (recentf-auto-cleanup 'never)
  (recentf-mode t)
  ;; Save history mode
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode t)
  ;; Save place mode
  (save-place-mode t)

;;;; Repeat mode
  (repeat-exit-key (kbd "C-g"))
  (repeat-exit-timeout 2)
  (repeat-mode t)

;;;; Dired
  (dired-listing-switches "-lahD --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer nil "Cannot open multiple dired windows if on")
  (dired-hide-details-hide-symlink-targets nil)
  (dired-hide-details-hide-information-lines nil)

;;;; Eshell
  (eshell-banner-message "")
  (eshell-history-size 1000)
  (eshell-save-history-on-exit t)
  (eshell-hist-ignoredups nil)
  (eshell-last-dir-ring-size 64)

;;;; ibuffer
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
                                  ("Git" (name . ,(pew::special-buffer 'magit)))
                                  ("VC" (name . ,(pew::special-buffer 'vc)))
                                  ("Ediff" (name . ,(pew::special-buffer 'ediff)))
                                  ;; Putting to last to avoid buffers being wrongly categorized as "special"
                                  ("Special" (starred-name)))))

;;;; isearch and search
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (search-highlight t)
  (search-highlight-submatches t)
  (query-replace-highlight t)
  (query-replace-lazy-highlight t)
  (query-replace-highlight-submatches t)
  (query-replace-show-replacement t)

;;;; ispell
  (ispell-dictionary "en_US")

;;;; ediff
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-vertically)

;;;; electric
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-mode t)
  (electric-indent-mode t)

;;;; TRAMP
  (tramp-default-method "scp")
  (tramp-remote-path '(tramp-own-remote-path tramp-default-remote-path))

;;;; Winner mode
  (winner-mode t)
  (winner-dont-bind-my-keys t)

;;; Switch commands
  :switch
  (indent-tabs-mode)
  (show-trailing-whitespace)
  (line-move-visual)
  (debug-on-error)
  (help-window-select)
  (display-line-numbers . (nil absolute relative visual))

;;; Transient keybindings
  :transient
  (pewkey
;;;; Take prefix
   ("C-u" . universal-argument)

;;;; Repeat
   ("C-r" . pewkey-repeat)

;;;; Windows
   ("q" . pew::close-window)
   ("1" . delete-other-windows)
   ("2" . split-window-below)
   ("3" . split-window-right)
   ("9" . window-toggle-side-windows)
   ("0" . pew::close-window)
   ("o" . pew::next-window)
   ("O" . pew::prev-window)
   ("h" . windmove-left)
   ("j" . windmove-down)
   ("k" . windmove-up)
   ("l" . windmove-right)
   ("C-<left>"  . shrink-window-horizontally)
   ("C-<down>"  . shrink-window)
   ("C-<up>"    . enlarge-window)
   ("C-<right>" . enlarge-window-horizontally)

;;;; Layout
   ("y" . winner-undo)
   ("Y" . winner-redo)

;;;; Other window
   ("C-v" . pew::scroll-other-window-page-down)
   ("M-v" . pew::scroll-other-window-page-up)
   ("C-n" . pew::scroll-other-window-line-down)
   ("C-p" . pew::scroll-other-window-line-up)
   ("C-l" . pew::recenter-other-window)

;;;; Tabbar
   ("Q"   . tab-bar-close-tab)
   ("R"   . tab-bar-rename-tab)
   ("f"   . tab-bar-switch-to-next-tab)
   ("F"   . tab-bar-select-tab-by-name)
   ("b"   . tab-bar-switch-to-prev-tab)
   ("t"   . tab-bar-new-tab)
   ("T"   . pew::pop-window-in-new-tab)
   ("C-t" . pew::pop-window-in-new-tab-persist)
   ("m"   . pew::move-tab-next)
   ("M"   . pew::move-tab-prev)

;;;; Buffers
   ("r" . rename-buffer)
   ("w" . save-buffer)
   ("n" . pew::next-editing-buffer)
   ("p" . pew::previous-editing-buffer)
   ("i" . pew::display-buffer-path)
   ("B" . display-buffer)
   ("g" . revert-buffer-quick)

;;;; Jump
   ("C-o" . pop-global-mark)
   ("."   . xref-find-definitions)
   ("?"   . xref-find-references)
   ("'"   . xref-find-apropos)
   ("C-x" . exchange-point-and-mark)
   ("SPC" . set-mark-command)

;;;; Edit
   ("u" . undo)
   ("U" . undo-redo)
   (";" . comment-line)
   ("/" . isearch-forward-regexp)
   ("," . isearch-query-replace-regexp)

;;;; Zoom (zooming in/out depends on the last key.  see `text-scale-adjust')
   ("C-=" . text-scale-adjust)
   ("C--" . text-scale-adjust)
   ("C-0" . text-scale-adjust)

;;;; Frame Transparency
   ("M-=" . pew::increase-frame-opacity)
   ("M--" . pew::decrease-frame-opacity)
   ("A"   . pew::pop-window-in-new-frame)
   ("C-a" . pew::pop-window-in-new-frame-persist)
   ("a"   . other-frame)

;;;; Rebind word manipulations
   ("M-t" . transpose-words)
   ("M-c" . capitalize-word)
   ("M-u" . upcase-word)
   ("M-l" . downcase-word)
   ("M-z" . zap-to-char)
   ("M-q" . fill-paragraph)
   ("M-h" . mark-paragraph)

;;;; Editing
   ("DEL" . cycle-spacing))

  :map
  (pew::M-o-map)
  (pew::M-t-map)
  (pew::M-c-map)
  (pew::M-u-map
   ("c" . org-capture)
   ("a" . org-agenda)
   ("! !" . flymake-mode)
   ("! b" . flymake-show-buffer-diagnostics)
   ("! p" . flymake-show-project-diagnostics))
  (pew::M-l-map)
  (pew::M-z-map)
  (pew::M-q-map)
  (pew::M-h-map)

;;; Mode keybindings
  :bind
;;;; Global
  (global-map
   ;; File and directory browsing
   ("C-x C-d" . dired-jump)

   ;; Remap for better experience
   ([remap next-buffer] . pew::next-editing-buffer)
   ([remap previous-buffer] . pew::previous-editing-buffer)
   ([remap list-buffers] . ibuffer)
   ([remap isearch-delete-char] . isearch-del-char)

   ;; Swap dabbrev default bindings
   ("M-/" . dabbrev-completion)
   ("C-M-/" . dabbrev-expand)

   ;; Pewkey
   ("C-z" . pewkey-map)

   ;; Less frequently used prefix that can be overriden
   ;; Reserved for the future
   ("M-o" . pew::M-o-map) ;; taken by minibuffer
   ("M-t" . pew::M-t-map)
   ("M-c" . pew::M-c-map) ;; taken by completion
   ("M-u" . pew::M-u-map) ;; taken by utilities
   ("M-l" . pew::M-l-map)
   ("M-z" . pew::M-z-map)
   ("M-q" . pew::M-q-map)
   ("M-h" . pew::M-h-map))

;;;; Dired
  (dired-mode-map
   ("RET" . pew::dired-go-to)
   ("DEL" . pew::dired-go-up)
   ("f" . dired-find-file)
   ("b" . dired-up-directory)
   ("<left>" . dired-up-directory)
   ("<right>" . dired-find-file))

;;; Mode hooks
  :hook
  ;; Make shell clean
  (eshell-mode-hook . pew::terminal-mode-oninit)
  (shell-mode-hook . pew::terminal-mode-oninit)

  ;; Don't save trailing spaces
  (after-save-hook . pew::delete-trailing-whitespaces)

  ;; Don't move cursor to the minibuffer prompt
  (minibuffer-setup-hook . cursor-intangible-mode)

  ;; Don't spawn new windows
  (grep-mode-hook . pew::reuse-window-in-buffer)

  ;; Enable folding
  (prog-mode-hook . outline-minor-mode)

  ;; Show less in Dired
  (dired-mode-hook . dired-hide-details-mode)

;;; Symbol properties
  :property
  ;; Enable commands that are disabled by default
  (scroll-left
   (disabled . nil))

  (list-threads
   (disabled . nil))

  (list-timers
   (disabled . nil))

  (dired-find-alternate-file
   (disabled . nil))

  (upcase-region
   (disabled . nil))

  (downcase-region
   (disabled . nil))

  (narrow-to-region
   (disabled . nil))

;;; Face settings
  :face
  (default :family "Iosevka" :foundry "UKWN" :slant normal :weight normal :height 120 :width normal)
  (tab-bar :inherit default))

(provide 'init-defaults)
;;; init-defaults.el ends here
