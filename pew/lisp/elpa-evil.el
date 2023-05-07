;;; elpa-evil.el --- Vim layer -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil provides vim-like keybindings and functionalities, which dramatically improves coding efficiency.
;; This file configures `evil-mode' related stuff including bringing in supplementary packages.

;;; Code:
;;; Evil
(use-package evil
  :demand t

;;;; Evil custom
  :custom
  ;; Default evil-want behavior
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-minibuffer nil)
  (evil-want-Y-yank-to-eol t)
  (evil-want-C-g-bindings nil)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-scroll nil)
  (evil-want-C-d-scroll nil)
  (evil-want-C-u-delete nil)
  (evil-want-C-h-delete nil)
  (evil-want-C-w-delete nil)
  (evil-want-C-w-in-emacs-state nil)
  (evil-want-abbrev-expand-on-insert-exit t)
  (evil-want-change-word-to-end t)
  (evil-want-empty-ex-last-command t)
  (evil-want-fine-undo t)
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
  ;; Initial state with less suprise
  ;; Use my own initial state setter instead.
  (evil-default-state 'pewinitial)
  (evil-motion-state-modes nil)
  (evil-normal-state-modes nil)
  (evil-insert-state-modes nil)
  (evil-visual-state-modes nil)
  (evil-replace-state-modes nil)
  (evil-operator-state-modes nil)
  (evil-buffer-regexps nil)

  :config
;;;; Evil keybinding functions
  ;; Key binding function
  (defun pew::evil::set-key (state map leader bindings)
    "A function to bind Evil keys.
This is basically a wrapper of `evil-define-key*'.
STATE is a Evil state symbol of a list of symbols.
MAP can be a map symbol or a list of symbols.
LEADER is non-nil, the BINDINGS will be prefixed with Evil leader key.
BINDINGS is an alist in the form of
  ((KEY . DEF) (KEY . DEF) ...)
See `evil-define-key*'."
    (declare (indent 3))
    (let ((l:bindings (mapcan (if leader
                                  (lambda (x) (list (kbd (concat "<leader>" (car x))) (cdr x)))
                                (lambda (x) (list (kbd (car x)) (cdr x))))
                              bindings)))
      (mapc (lambda (m) (apply 'evil-define-key* state m l:bindings))
            (if (listp map) map (list map)))))

;;;; Evil search functions
  ;; This search action searches words selected in visual mode, escaping any special
  ;; characters. Also it provides a quick way to substitute the words just searched.
  (defun pew::evil::escape-pattern (pattern)
    "Escape special characters in PATTERN which is used by evil search."
    (if (zerop (length pattern)) pattern
      ;; `regexp-quote' does not escape /
      (replace-regexp-in-string "/" "\\\\/" (regexp-quote pattern))))

  (defun pew::evil::search-region-text (beg end)
    "Use evil-search for text in the region from BEG to END."
    ;; Copy region text
    (setq evil-ex-search-pattern
          (evil-ex-make-pattern
           (pew::evil::escape-pattern (buffer-substring-no-properties beg end))
           'sensitive
           t))
    (evil-yank beg end)
    (ignore-error 'search-failed
      (evil-ex-search-next)))

  (defun pew::evil::visual-search-region-text ()
    "Search the text selected in visual state."
    (interactive)
    (when (evil-visual-state-p)
      (setq evil-ex-search-count 1)
      (setq evil-ex-search-direction 'forward)
      (when (pew::evil::search-region-text (region-beginning) (region-end))
        (evil-ex-search-previous))
      (evil-normal-state)))

  (defun pew::evil::replace-last-search ()
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

  (defun pew::evil::search-word()
    "Search and highlight the word under cursor but don't jumpt to the next."
    (interactive)
    (evil-ex-search-word-forward)
    (evil-ex-search-previous))

;;;; Workaround
  ;; Evil X settings
  ;; Don't allow Evil to kill selected region when yanking
  ;; See: https://emacs.stackexchange.com/questions/14940/evil-mode-visual-selection-copies-text-to-clipboard-automatically/15054#15054
  (define-advice evil-visual-update-x-selection (:override (&rest _args) pew::evil::visual-update-x-selection))

;;;; State tags (Cannot be set by customize)
  (setq evil-emacs-state-tag "EMACS"
        evil-normal-state-tag "NORMAL"
        evil-insert-state-tag "INSERT"
        evil-replace-state-tag "REPLACE"
        evil-visual-char-tag "VISUAL"
        evil-visual-line-tag "VISUAL"
        evil-visual-block-tag "VISUAL"
        evil-visual-screen-line-tag "VISUAL"
        evil-motion-state-tag "MOTION"
        evil-operator-state-tag "OPERATOR")

;;;; Evil initial states
  (evil-define-state pewinitial
    "A dummy state used to determine buffer initial Evil state.
NOTE: This dummy state means to be an intermidiate state which transits to
another legit Evil state immediately under different conditions.  Due to the
limitation in which the state toggle function can't switch to another state,
the body of this state toggle is empty and the actual transition is done by
an advice."
    :tag "PEWINIT"
    :message "-- PEWINIT --")

  (defvar pew::evil::initial-state-plist
    `(:minor ((view-mode . motion))
      :major ((messages-buffer-mode . motion)
              (help-mode . motion)
              (image-mode . motion)
              (view-mode . motion))
      :name ((,(pew::special-buffer '(scratch edit-indirect org-starred)) . normal)))
    "A plist to determine buffer initial state by different conditions.
The precedence of the effectiveness is: Minor, Major, Name.")

  (define-advice evil-pewinitial-state (:after (&rest arg) pew::evil::initial-state)
    "Advice to alter `evil-pewinitial-state' toggle behavior.  This advice works
in conjunction with the toggle to decide a buffer's initial Evil state.
This is an advanced method to determine initial state rather than using
`evil-set-initial-state' and `evil-buffer-regexps'."
    (if (and (numberp arg) (< arg 1))
        nil ;; Don't interfere toggle off.
      (let ((l:to-toggle (lambda (s)
                           (intern (format "evil-%s-state" s))))
            (l:select-first (lambda (checker keyword)
                              (car-safe (delq nil (mapcar
                                                   checker
                                                   (plist-get pew::evil::initial-state-plist keyword))))))
            (l:state-toggle nil))
        (cond
         ;; State by rules
         ((setq l:state-toggle
                (or
                 ;; State by minor mode
                 ;; TODO: Currently bugs due to the delay of the minor mode variable setting.
                 (funcall l:select-first (lambda (cons)
                                           (and (symbolp (car cons))
                                                (symbol-value (car cons))
                                                (funcall l:to-toggle (cdr cons))))
                          :minor)
                 ;; State by major mode
                 (funcall l:select-first (lambda (cons)
                                           (and (eq major-mode (car cons))
                                                (funcall l:to-toggle (cdr cons))))
                          :major)
                 ;; State by buffer name
                 (funcall l:select-first (lambda (cons)
                                           (and (string-match-p (car cons) (buffer-name))
                                                (funcall l:to-toggle (cdr cons))))
                          :name)))
          (funcall l:state-toggle 1))

         ;; General editable buffer
         ((and (pew::special-buffer-match-p 'non-starred (buffer-name))
               (not buffer-read-only))
          (evil-normal-state 1))

         ;; Default buffer state
         (t
          (evil-emacs-state 1))))))

;;;; Evil keybindings
  ;; Toggle key
  (evil-set-toggle-key "C-x m")
  (global-set-key (kbd "C-x C-m") #'evil-mode)

  ;; Leader keys
  (evil-set-leader '(normal motion) (kbd "\\")) ;; <leader>
  ;;(evil-set-leader '(normal motion) (kbd "\\") 'localleader) ;; <localleader>

  ;; Normal and motion state bindings with leader key
  (pew::evil::set-key '(normal motion) 'global :leader
    ;; Search and substitution
    '(("cs" . pew::evil::replace-last-search)))

  ;; Normal and motion state bindings
  (pew::evil::set-key '(normal motion) 'global nil
    '(("SPC" . pewkey)
      ;; Search
      ("#" . evil-ex-nohighlight)
      ("*" . pew::evil::search-word)))

  ;; Visual state bindings
  (pew::evil::set-key 'visual 'global nil
    ;; Search
    '(("*" . pew::evil::visual-search-region-text)))

  ;; Elisp with leader
  (with-eval-after-load 'elisp-mode
    (pew::evil::set-key '(visual normal) (list emacs-lisp-mode-map lisp-interaction-mode-map) :leader
      ;; Quick eval
      '(("eb" . eval-buffer)
        ("er" . eval-region)
        ("ef" . eval-defun)
        ("ee" . eval-last-sexp))))

;;;; Enable Evil
  (evil-mode 1)) ;; (use-package evil)

;;; Evil surround
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(provide 'elpa-evil)
;;; elpa-evil.el ends here
