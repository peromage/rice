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

;;;; Evil keybinding functions
  ;; Key binding function
  (defun pew::evil::set-key (state map leader bindings)
    "A function to bind Evil keys.
This is basically a wrapper of `evil-define-key*'.
STATE is a Evil state symbol of a list of symbols.
MAP can be a map symbol or a list of symbols.
LEADER is non-nil, the BINDINGS will be prefixed with Evil leader key.
BINDINGS is a list of cons in the form of
  ((KEY . DEF) (KEY . DEF) ...)
See `evil-define-key*'."
    (declare (indent 3))
    (let ((l:bindings (mapcan (if leader
                                  (lambda (x) (list (kbd (concat "<leader>" (car x))) (cdr x)))
                                (lambda (x) (list (kbd (car x)) (cdr x))))
                              bindings)))
      (mapc (lambda (m) (apply 'evil-define-key* state m l:bindings))
            (if (listp map) map (list map)))))

;;;; Evil state setting functions
  ;; Initial state function
  (defun pew::evil::set-mode-state (states)
    "Set initial STATES for major or minor modes.
STATES is a list of cons:
  ((MODE . STATE) (MODE . STATE) ...)
The MODE and STATE will be completed to their full names.
Major mode uses `evil-set-initial-state' which is equivalent to:
  (evil-set-initial-state MODE STATE)
Minor mode uses `add-hook' which is equivalent to:
  (add-hook 'MODE-hook #'evil-STATE-state)"
    (declare (indent 0))
    (mapc (lambda (x) (let ((l:mode (car x))
                            (l:state (cdr x)))
                        (if (memq l:mode minor-mode-list)
                            (add-hook (intern (format "%s-hook" l:mode)) (intern (format "evil-%s-state" l:state)))
                          (evil-set-initial-state l:mode l:state))))
          states))

  (defun pew::evil::set-buffer-state (states)
    "Set initial STATES for certain buffer names.
STATES is a list of cons:
  ((NAME . STATE) (NAME . STATE) ...)
The earlier the buffer name in the list the higher priority it has.
See `evil-buffer-regexps'.
NOTE: Buffer name patterns takes precedence over the mode based methods."
    (declare (indent 0))
    (setq evil-buffer-regexps (append states evil-buffer-regexps)))

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

;;;; Evil initial states
  ;; NOTE: This takes precedence over the mode initial states below
  (pew::evil::set-buffer-state
    ;; VC buffers
    `((,(pew::special-buffer 'vc) . emacs)
      (,(pew::special-buffer 'magit) . emacs)
      (,(pew::special-buffer 'ediff) . emacs)
      ;; Shell buffers
      (,(pew::special-buffer 'shell) . emacs)
      (,(pew::special-buffer 'terminal) . emacs)
      ;; Buffers in motion
      (,(pew::special-buffer 'help) . motion)
      (,(pew::special-buffer 'message) . motion)
      (,(pew::special-buffer 'backtrace) . motion)
      (,(pew::special-buffer 'warning) . motion)
      (,(pew::special-buffer 'log) . motion)
      (,(pew::special-buffer 'compilation) . motion)
      (,(pew::special-buffer 'output) . motion)
      (,(pew::special-buffer 'command) . motion)
      (,(pew::special-buffer 'man) . motion)
      ;; Buffer in normal
      (,(pew::special-buffer 'scratch) . normal)
      (,(pew::special-buffer 'org-src) . normal)
      (,(pew::special-buffer 'org-export) . normal)
      (,(pew::special-buffer 'edit-indirect) . normal)
      ;; Fallback initial state for all special buffers
      (,(pew::special-buffer 'starred) . emacs)))

  (pew::evil::set-mode-state
    ;; Major modes
    '((dired-mode . emacs)
      (image-mode . emacs)
      (help-mode . motion)
      (message-mode . motion)
      (compilation-mode . motion)
      (vterm-mode . emacs)
      ;; Minor modes
      (view-mode . motion)))

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
        ("ee" . eval-last-sexp))))) ;; (use-package evil)

;;; Evil surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(provide 'elpa-evil)
;;; elpa-evil.el ends here
