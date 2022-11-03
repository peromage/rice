;;; elpa-evil.el --- Vim layer -*- lexical-binding: t -*-

;;; Commentary:
;; Evil provides vim-like keybindings and functionalities, which dramatically improves coding efficiency.
;; This file configures `evil-mode' related stuff including bringing in supplementary packages.

;;; Code:

;;; Evil
(use-package evil
  :demand t
  :init
;;;; Evil keybinding functions
  ;; Key binding function
  (defun pew/evil/set-key (state map prefix &rest bindings)
    "Set BINDINGS with PREFIX in MAP for STATE.
STATE is one of 'normal, 'insert, 'visual, 'replace, 'operator, 'motion,
'emacs, a list of one or more of these, or nil, which means all of the above.
MAP is either a single map or a list of maps.
PREFIX could be nil or a string of KEY.
BINDINGS is a list of the form:
  (KEY DEF KEY DEF ...)
The arguments will be collected in pairs and passed to `evil-define-key'."
    (declare (indent 3))
    (if (pew/oddp (length bindings))
        (error "Incomplete keys and definitions"))
    (let ((l/bindings bindings)
          (l/result nil))
      (while l/bindings
        (push (kbd (concat prefix (pop l/bindings))) l/result)
        (push (pop l/bindings) l/result))
      (setq l/result (reverse l/result))
      (let ((l/setter (lambda (m) (apply 'evil-define-key* state m l/result))))
        (cond ((keymapp map)
               (funcall l/setter map))
              ((listp map)
               (mapc l/setter map))
              ;; The map could be a symbol i.e. 'global
              (t
               (funcall l/setter map))))))

;;;; Evil state setting functions
  ;; Initial state function
  (defun pew/evil/set-mode-state (&rest states)
    "Set initial STATES for major or minor modes.
STATES is a list of the form:
  (MODE STATE MODE STATE ...)
The MODE and STATE will be completed to their full names.
Major mode uses `evil-set-initial-state' which is equivalent to:
  (evil-set-initial-state MODE STATE)
Minor mode uses `add-hook' which is equivalent to:
  (add-hook 'MODE-hook #'evil-STATE-state)"
    (declare (indent 0))
    (if (pew/oddp (length states))
        (error "Incomplete modes and states"))
    (let ((l/states states))
      (while l/states
        (let ((l/mode (pop l/states))
              (l/state (pop l/states)))
          (cond ((memq l/mode minor-mode-list)
                 (add-hook (intern (format "%s-hook" l/mode)) (intern (format "evil-%s-state" l/state))))
                (t
                 (evil-set-initial-state l/mode l/state)))))))

  (defun pew/evil/set-buffer-state (&rest states)
    "Set initial STATES for certain buffer names.
STATES is a list of the form:
  (REG STATE REG STATE ...)
The later buffer regex will have higher priority.
Equivalent to:
  (push '(REG . STATE) evil-buffer-regexps)
NOTE: Buffer name patterns takes precedence over the mode based methods."
    (declare (indent 0))
    (if (pew/oddp (length states))
        (error "Incomplete patterns and states"))
    ;; Backwards iterating so that the order is consistent with the written list
    (let ((l/states (reverse states)))
      (while l/states
        (let ((l/state (pop l/states))
              (l/regex (pop l/states)))
          (push (cons l/regex l/state) evil-buffer-regexps)))))

;;;; Evil search functions
  ;; This search action searches words selected in visual mode, escaping any special
  ;; characters. Also it provides a quick way to substitute the words just searched.
  (defun pew/evil/escape-pattern (pattern)
    "Escape special characters in PATTERN which is used by evil search."
    (if (zerop (length pattern)) pattern
      ;; `regexp-quote' does not escape /
      (replace-regexp-in-string "/" "\\\\/" (regexp-quote pattern))))

  (defun pew/evil/search-region-text (beg end)
    "Use evil-search for text in the region from BEG to END."
    ;; Copy region text
    (setq evil-ex-search-pattern
          (evil-ex-make-pattern
           (pew/evil/escape-pattern (buffer-substring-no-properties beg end))
           'sensitive
           t))
    (evil-yank beg end)
    (ignore-error 'search-failed
      (evil-ex-search-next)))

  (defun pew/evil/visual-search-region-text ()
    "Search the text selected in visual state."
    (interactive)
    (when (evil-visual-state-p)
      (setq evil-ex-search-count 1)
      (setq evil-ex-search-direction 'forward)
      (when (pew/evil/search-region-text (region-beginning) (region-end))
        (evil-ex-search-previous))
      (evil-normal-state)))

  (defun pew/evil/replace-last-search ()
    "Replace the last Evil EX search."
    (interactive)
    (if (not evil-ex-search-pattern)
        (user-error "No search pattern found"))
    ;; Substitute from current position
    (evil-ex-substitute
     (point)
     (point-max)
     evil-ex-search-pattern
     (read-string (concat (car evil-ex-search-pattern) " -> "))
     (list ?g ?c)))

;;;; Evil X settings
  ;; Don't allow Evil to kill selected region when yanking
  ;; See: https://emacs.stackexchange.com/questions/14940/evil-mode-visual-selection-copies-text-to-clipboard-automatically/15054#15054
  (define-advice evil-visual-update-x-selection (:override (&rest _args) pew/evil/visual-update-x-selection))

;;;; Evil custom
  :custom
  (evil-want-integration t)
  (evil-want-keybinding t)
  (evil-want-minibuffer nil)
  (evil-want-Y-yank-to-eol t)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-scroll nil)
  (evil-want-C-d-scroll nil)
  (evil-want-C-u-delete nil)
  (evil-want-C-g-bindings nil)
  (evil-want-C-h-delete nil)
  (evil-want-C-w-delete nil)
  (evil-want-C-w-in-emacs-state nil)
  (evil-disable-insert-state-bindings t)
  (evil-cross-lines nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-auto-balance-windows t)
  (evil-ex-search-highlight-all t)
  (evil-ex-search-persistent-highlight t)
  (evil-symbol-word-search nil)
  (evil-kill-on-visual-paste t)
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-redo)
  ;; Initial states for major modes
  (evil-default-state 'normal)
  ;; Make less suprise and keep `evil-emacs-state-modes' as is
  (evil-motion-state-modes nil)
  (evil-normal-state-modes nil)
  (evil-insert-state-modes nil)
  (evil-visual-state-modes nil)
  (evil-replace-state-modes nil)
  (evil-operator-state-modes nil)
  (evil-buffer-regexps nil)
  :config
  (evil-mode 1)

;;;; Evil initial states
  ;; NOTE: This takes precedence over the mode initial states below
  (pew/evil/set-buffer-state
    ;; VC buffers
    (pew/special-buffer vc) 'emacs
    (pew/special-buffer magit) 'emacs
    (pew/special-buffer ediff) 'emacs
    ;; Shell buffers
    (pew/special-buffer shell) 'emacs
    (pew/special-buffer terminal) 'emacs
    ;; Buffers in motion
    (pew/special-buffer help) 'motion
    (pew/special-buffer message) 'motion
    (pew/special-buffer backtrace) 'motion
    (pew/special-buffer warning) 'motion
    (pew/special-buffer log) 'motion
    (pew/special-buffer compilation) 'motion
    (pew/special-buffer output) 'motion
    (pew/special-buffer command) 'motion
    (pew/special-buffer man) 'motion
    ;; Buffer in normal
    (pew/special-buffer scratch) 'normal
    (pew/special-buffer org-src) 'normal
    ;; Fallback initial state for all special buffers
    (pew/special-buffer starred) 'emacs)

  (pew/evil/set-mode-state
    ;; Major modes
    'dired-mode 'emacs
    'image-mode 'emacs
    'help-mode 'motion
    'message-mode 'motion
    'compilation-mode 'motion
    'vterm-mode 'emacs
    ;; Minor modes
    'view-mode 'motion)

;;;; Evil keybindings
  ;; Leader keys
  (evil-set-leader '(normal motion) (kbd "SPC")) ;; <leader>
  (evil-set-leader '(normal motion) (kbd "\\") 'localleader) ;; <localleader>

  ;; Normal and motion state bindings
  (pew/evil/set-key '(normal motion) 'global "<leader>"
    ;; Windows
    "q" #'pew/close-window
    "h" #'evil-window-left
    "j" #'evil-window-down
    "k" #'evil-window-up
    "l" #'evil-window-right
    "s" #'evil-window-split
    "v" #'evil-window-vsplit

    ;; Tabs
    "Q" #'tab-bar-close-tab
    "R" #'tab-bar-rename-tab
    "t" #'tab-bar-new-tab
    "T" #'pew/pop-window-in-new-tab
    "f" #'tab-bar-switch-to-next-tab
    "b" #'tab-bar-switch-to-prev-tab
    "m" #'pew/move-tab-next
    "M" #'pew/move-tab-prev

    ;; Buffers
    "r" #'rename-buffer
    "w" #'save-buffer
    "n" #'next-buffer
    "p" #'previous-buffer
    "g" #'pew/buffer-full-path
    "B" #'display-buffer

    ;; Jump
    "o" #'evil-jump-backward
    "O" #'evil-jump-forward

    ;; Search and substitution
    "cs" #'pew/evil/replace-last-search)

  (pew/evil/set-key '(normal motion) 'global nil
    ;; Windows
    "<left>" #'evil-window-decrease-width
    "<down>" #'evil-window-decrease-height
    "<up>" #'evil-window-increase-height
    "<right>" #'evil-window-increase-width

    ;; Search
    "#" #'evil-ex-nohighlight)

  ;; Visual state bindings
  (pew/evil/set-key 'visual 'global nil
    ;; Search
    "*" #'pew/evil/visual-search-region-text)

  (with-eval-after-load 'elisp-mode
    (pew/evil/set-key '(visual normal) (list emacs-lisp-mode-map lisp-interaction-mode-map) "<leader>"
      ;; Quick eval
      "eb" #'eval-buffer
      "er" #'eval-region
      "ef" #'eval-defun
      "ee" #'eval-last-sexp)))

(provide 'elpa-evil)
;;; elpa-evil.el ends here
