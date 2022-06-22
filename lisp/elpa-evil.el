;;; elpa-evil.el --- Vim layer -*- lexical-binding: t -*-
;;; Commentary:
;; Evil provides vim-like keybindings and functionalities, which dramatically improves coding efficiency.
;; This file configures `evil-mode' related stuff including bringing in supplementary packages.

;;; Code:
;;; Evil
(use-package evil
  :demand t
  :init
;;;; Evil utilities
  ;; Key binding function
  (defun pew/evil/set-key (state map prefix &rest bindings)
    "Set BINDINGS with PREFIX in MAP for STATE.
STATE is one of 'normal, 'insert, 'visual, 'replace, 'operator, 'motion,
'emacs, a list of one or more of these, or nil, which means all of the above.
PREFIX could be nil or a string of KEY.
BINDINGS is a list of the form:
  (KEY DEF KEY DEF ...)
The arguments will be collected in pairs and passed to `evil-define-key'.
"
    (declare (indent 3))
    (if (pew/oddp (length bindings))
        (error "Incomplete keys and definitions"))
    (let ((bindings_ bindings))
      (while bindings_
        (setcar bindings_ (kbd (concat prefix (pop bindings_))))
        (pop bindings_))
      (apply 'evil-define-key* state map bindings)))

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
    (let ((states_ states))
      (while states_
        (let ((mode_ (pop states_))
              (state_ (pop states_)))
          (cond ((memq mode_ minor-mode-list)
                 (add-hook (intern (format "%s-hook" mode_)) (intern (format "evil-%s-state" state_))))
                (t
                 (evil-set-initial-state mode_ state_)))))))

  (defun pew/evil/set-buffer-state (&rest states)
    "Set initial STATES for certain buffer names.
STATES is a list of the form:
  (REG STATE REG STATE ...)
The later buffer regex will have higher priority.
Equivalent to:
  (push '(REG . STATE) evil-buffer-regexps)
NOTE: Setting by buffer name patterns takes precedence over the mode based methods."
    (declare (indent 0))
    (if (pew/oddp (length states))
        (error "Incomplete patterns and states"))
    ;; Backwards iterating so that the order is consistent with the written list
    (let ((states_ (reverse states)))
      (while states_
        (let ((state_ (pop states_))
              (regex_ (pop states_)))
          (push (cons regex_ state_) evil-buffer-regexps)))))

;;;; Evil search
  ;; This search action searches words selected in visual mode, escaping any special
  ;; characters. Also it provides a quick way to substitute the words just searched.
  (defun pew/evil/escape-region (begin end)
    "Escape special chars in region from BEGIN to END for evil-search mode."
    (catch 'return_
      (let ((region_ (buffer-substring-no-properties begin end))
            (placeholder_ "_IM_A_PERCENTAGE_"))
        (if (= (length region_) 0)
            (throw 'return_ nil))
        ;; Replace the % symbols so that `regexp-quote' does not complain
        (setq region_ (replace-regexp-in-string "%" placeholder_ region_))
        (setq region_ (regexp-quote region_))
        ;; `regexp-quote' does not escape /. We escape it here so that evil-search
        ;; can recognize it
        (setq region_ (replace-regexp-in-string "/" "\\\\/" region_))
        ;; Change the % symbols back
        (setq region_ (replace-regexp-in-string placeholder_ "%" region_))
        (throw 'return_ region_))))

  (defun pew/evil/search-region ()
    "Use evil-search for the selected region."
    (when (use-region-p)
      (setq evil-ex-search-count 1
            evil-ex-search-direction 'forward)
      ;; Copy region text
      (evil-yank (region-beginning) (region-end))
      (let* ((quoted-pattern_ (pew/evil/escape-region (region-beginning) (region-end)))
             (result_ (evil-ex-search-full-pattern quoted-pattern_ evil-ex-search-count evil-ex-search-direction))
             (success_ (pop result_))
             (pattern_ (pop result_))
             (offset_ (pop result_)))
        (when success_
          ;; From `evil-ex-start-search'
          (setq evil-ex-search-pattern pattern_
                evil-ex-search-offset offset_)
          ;; `evil-ex-search-full-pattern' jumps to the end of the next one if there
          ;; are more than one candidates. So we jump twice here to go back to the
          ;; very first one that we selected.
          (evil-ex-search-previous)
          (evil-ex-search-previous)))))

  (defun pew/evil/visual-search-region ()
    "Search the selected region visual state and return to normal state."
    (interactive)
    (when (evil-visual-state-p)
      (pew/evil/search-region)
      (evil-normal-state)))

  ;; TODO: Fix the register calls
  (defun pew/evil/normal-search-at-point ()
    "Search word under the cursor in normal mode."
    (interactive)
    (evil-ex-search-word-forward)
    (evil-set-register "*" (evil-get-register "/")))

  (defun pew/evil/replace-last-search ()
    "Replace the PATTERN with REPLACEMENT, which is currently searched by evil ex search."
    (interactive)
    (if (not evil-ex-search-pattern)
        (user-error "No search pattern found"))
    (let* ((regex_ (nth 0 evil-ex-search-pattern))
           (replacement_ (read-string (concat regex_ " -> ")))
           (flags_ (list ?g ?c))
           (subpattern_ (evil-ex-make-substitute-pattern regex_ flags_)))
      ;; Substitute from current position
      (evil-ex-substitute (point) (point-max) subpattern_ replacement_ flags_)))

  ;; Don't allow Evil to kill selected region when yanking
  ;; See: https://emacs.stackexchange.com/questions/14940/evil-mode-visual-selection-copies-text-to-clipboard-automatically/15054#15054
  (define-advice evil-visual-update-x-selection (:override (&rest args) pew/evil/visual-update-x-selection))

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
   "^ *\\*vc-.*\\*" 'emacs
   "^ *magit" 'emacs
   ;; Motion buffers
   "^ *\\*.*[Hh]elp\\*" 'motion
   "^ *\\*.*[Mm]essages\\*" 'motion
   "^ *\\*.*[Bb]acktrace\\*" 'motion
   "^ *\\*.*[Ww]arnings\\*" 'motion
   "^ *\\*.*[Ll]og\\*" 'motion
   "^ *\\*.*[Cc]ompilation\\*" 'motion
   ;; Normal buffers
   "^ *\\*[Ss]cratch\\*" 'normal
   "^ *\\*.*[Ss]hell\\*" 'normal
   "^ *\\*.*[Tt]erm\\(inal\\)?\\*" 'normal
   "^ *\\*[Oo]rg [Ss]rc .*\\*" 'normal
   ;; Fallback initial state for all special buffers
   "^ *\\*.*\\*" 'emacs)

  (pew/evil/set-mode-state
   ;; Major modes
   'dired-mode 'emacs
   'help-mode 'motion
   'message-mode 'motion
   'compilation-mode 'motion
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
   "1" #'delete-other-windows
   "2" #'split-window-below
   "3" #'split-window-right
   "0" #'delete-window

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
   "g" #'pew/buffer-full-path

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
   "*" #'pew/evil/visual-search-region)

  (eval-after-load 'elisp-mode
    (let ((bindings_ (list
     ;; Quick eval
     "eb" #'eval-buffer
     "er" #'eval-region
     "ef" #'eval-defun
     "ee" #'eval-last-sexp)))
      (apply 'pew/evil/set-key '(visual normal) emacs-lisp-mode-map "<leader>" bindings_)
      (apply 'pew/evil/set-key '(visual normal) lisp-interaction-mode-map "<leader>" bindings_))))

(provide 'elpa-evil)
;;; elpa-evil.el ends here
