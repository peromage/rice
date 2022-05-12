;;; elpa-evil.el --- Vim layer -*- lexical-binding: t -*-
;;; Commentary:

;; Evil provides vim-like keybindings and functionalities, which dramatically improves coding efficiency.
;; This file configures `evil-mode' related stuff including bringing in supplementary packages.

;;; Code:
;;;; Evil setup

(use-package evil
  :demand t
  :init
  ;; Key binding macro
  (defmacro pew/evil/set-key (state map leader &rest bindings)
    "A helper macro bind BINDINGS with MAP in Evil STATE.
LEADER is one of 'leader, 'localleader or nil representating binding
with global leader key, local leader key or no leader key respectively.
STATE is one of 'normal, 'insert, 'visual, 'replace, 'operator, 'motion,
'emacs, a list of one or more of these, or nil, which means all of the above.
BINDINGS is a list of cons consisting keys and bound functions:
  (pew/evil/set-key-with-leader '(normal motion) 'global nil
    \"key1\" #'func1
    \"key2\" #'func2
    \"key3\" #'func3)
    ...
Equivalent to:
  (evil-define-key '(normal motion) 'global
    \"<leader>key1\" #'func1
    \"<leader>key2\" #'func2
    \"<leader>key3\" #'func3
    ...)"
    (let ((bindings-copy bindings)
          ;; The leader specifier has to be "quoted"
          (prefix (cond ((and (consp leader) (eq 'leader (cadr leader))) "<leader>")
                        ((and (consp leader) (eq 'localleader (cadr leader))) "<localleader>")
                        (t ""))))
      (while bindings-copy
        (setcar bindings-copy (kbd (concat prefix (car bindings-copy))))
        (setq bindings-copy (cddr bindings-copy)))
      `(evil-define-key ,state ,map ,@bindings)))

  ;; Initial state for mode macro
  (defmacro pew/evil/set-initial-state (&rest states)
    "A helper macro that sets initial STATES for modes.
STATES is a list in the form of:
  (pew/evil/set-initial-state 'mode1 'normal
                              'mode2 'visual
                              'mode3 'insert
                              ...)
Equivalent to:
  (add-hook 'mode1-hook #'evil-normal-state)
  (add-hook 'mode2-hook #'evil-visual-state)
  (add-hook 'mode3-hook #'evil-insert-state)
  ...
The mode and state will be completed to their full names.
Since `evil-set-initial-state' doesn't work for minor modes, `add-hook' takes
care of the mode initial states.
NOTE: Buffer names defined in `evil-buffer-regexps' takes precedence over this."
    (if (pew/oddp (length states))
        (error "Incomplete mode and state pairs"))
    (let ((result '(progn))
          (hook-fmt "%s-hook")
          (state-fmt "evil-%s-state"))
      (while states
        (let ((mode (intern (format hook-fmt (cadr (pop states)))))
              (state (intern (format state-fmt (cadr (pop states))))))
          (push `(add-hook ',mode #',state) result)))
      (reverse result)))

  ;; Adjust Window closing behavior
  (defun pew/evil/close-window ()
    "Close window on conditional.  If there is only one window then close the tab."
    (interactive)
    (cond ((one-window-p)
           (tab-bar-close-tab)
           (previous-window))
          (t (delete-window))))

  ;; Evil search
  ;; This search action searches words selected in visual mode, escaping any special
  ;; characters. Also it provides a quick way to substitute the words just searched.

  (defun pew/evil/escape-region (begin end)
    "Escape region from BEGIN to END for evil-search mode."
    (catch 'result
      (let ((selection (buffer-substring-no-properties begin end))
            (placeholder "_IM_A_PERCENTAGE_"))
        (if (= (length selection) 0)
            (throw 'result nil))
        ;; Replace the % symbols so that `regexp-quote' does not complain
        (setq selection (replace-regexp-in-string "%" placeholder selection))
        (setq selection (regexp-quote selection))
        ;; `regexp-quote' does not escape /. We escape it here so that evil-search
        ;; can recognize it
        (setq selection (replace-regexp-in-string "/" "\\\\/" selection)
              ;; Change the % symbols back
              selection (replace-regexp-in-string placeholder "%" selection)))))

  (defun pew/evil/search-selected ()
    "Use evil-search for the selected region."
    (when (use-region-p)
      (setq evil-ex-search-count 1
            evil-ex-search-direction 'forward)
      (evil-yank (region-beginning) (region-end))
      (let* ((quoted-pattern (pew/evil/escape-region (region-beginning) (region-end)))
             (result (evil-ex-search-full-pattern
                      quoted-pattern
                      evil-ex-search-count
                      evil-ex-search-direction))
             (success (pop result))
             (pattern (pop result))
             (offset (pop result)))
        (when success
          ;; From `evil-ex-start-search'
          (setq evil-ex-search-pattern pattern
                evil-ex-search-offset offset)
          ;; `evil-ex-search-full-pattern' jumps to the end of the next one if there
          ;; are more than one candidates. So we jump twice here to go back to the
          ;; very first one that we selected.
          (evil-ex-search-previous)
          (evil-ex-search-previous)))))

  (defun pew/evil/visual-search-selected ()
    "Search the selected region visual state and return to normal state."
    (interactive)
    (when (evil-visual-state-p)
      (pew/evil/search-selected)
      (evil-normal-state)))

  ;; TODO: Fix the register calls
  (defun pew/evil/normal-search-cursor ()
    "Search word under the cursor in normal mode."
    (interactive)
    (evil-ex-search-word-forward)
    (evil-set-register "*" (evil-get-register "/")))

  (defun pew/evil/replace-last-search ()
    "Replace the PATTERN with REPLACEMENT, which is currently searched by evil ex search."
    (interactive)
    (if (not evil-ex-search-pattern)
        (user-error "No search pattern found"))
    (let* ((regex (nth 0 evil-ex-search-pattern))
           (replacement (read-string (concat regex " -> ")))
           (flags (list ?g ?c))
           (subpattern (evil-ex-make-substitute-pattern regex flags)))
      (message "regex %s" regex)
      (message "replacement %s" replacement)
      (message "subpattern %S" subpattern)
      (evil-ex-substitute (point-min) (point-max) subpattern replacement flags)))

  :custom
  (evil-want-integration t)
  (evil-want-keybinding t)
  (evil-want-minibuffer nil)
  (evil-want-Y-yank-to-eol t)
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
  ;; Set initial buffer states
  ;; NOTE: This takes precedence over mode initial states below
  (evil-buffer-regexps '(("*scratch*" . normal)
                         ("*Messages*" . motion)
                         ("*Help*" . motion)
                         ("\\`magit" . emacs)
                         ;; General special definitions go last
                         ("\\` *\\*.*\\*" . emacs)))
  :config
  (evil-mode 1)

  ;; Explicitly set the initial state for a major mode
  (pew/evil/set-initial-state

   'dired-mode 'emacs
   'view-mode 'motion
   'help-mode 'motion

   )

  ;; Leader keys
  (evil-set-leader '(normal motion) (kbd "SPC"))
  (evil-set-leader '(normal motion) (kbd "\\") 'localleader)

  ;; Normal and motion state bindings
  (pew/evil/set-key '(normal motion) 'global 'leader

   ;; Windows
   "q" #'pew/evil/close-window
   "h" #'evil-window-left
   "j" #'evil-window-down
   "k" #'evil-window-up
   "l" #'evil-window-right
   "s" #'evil-window-split
   "v" #'evil-window-vsplit

   ;; Tabs
   "t" #'tab-bar-new-tab
   "m" #'pew/move-tab-next
   "M" #'pew/move-tab-prev
   "r" #'tab-bar-rename-tab
   "T" #'pew/pop-window-in-new-tab
   "f" #'tab-bar-switch-to-next-tab
   "b" #'tab-bar-switch-to-prev-tab

   ;; Buffers
   "w" #'save-buffer
   "n" #'next-buffer
   "p" #'previous-buffer
   "g" #'pew/show-file-path

   ;; Search and substitution
   "cs" #'pew/evil/replace-last-search

   )

  (pew/evil/set-key '(normal motion) 'global nil

   ;; Windows
   "<left>" #'evil-window-decrease-width
   "<down>" #'evil-window-decrease-height
   "<up>" #'evil-window-increase-height
   "<right>" #'evil-window-increase-width

   ;; Search
   "#" #'evil-ex-nohighlight

   )

  ;; Visual state bindings
  (pew/evil/set-key 'visual 'global nil

   ;; Search
   "*" #'pew/evil/visual-search-selected

   ))

;;;; Evil enhancement

;; Extend Evil keybindings to more modes
(use-package evil-collection
  :disabled
  :custom
  ;; NOTE: evil-collection assumes `evil-want-keybinding' is set to nil and
  ;; `evil-want-integration' is set to t before loading evil and evil-collection
  (evil-want-keybinding nil)
  (evil-want-integration t)
  :config
  ;; Enable evil collection for all support modes
  ;; Or simply do it for certain modes
  ;(evil-collection-init '(calendar dired calc ediff))
  (evil-collection-init))

;; Make Evil undo/redo easier
(use-package undo-tree
  :hook (evil-local-mode . turn-on-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (evil-set-undo-system 'undo-tree))

(provide 'elpa-evil)
;;; elpa-evil.el ends here
