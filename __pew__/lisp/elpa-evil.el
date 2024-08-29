;;; elpa-evil.el --- evil and complementary -*- lexical-binding: t; -*-
;;; Commentary:

;; NOTE: There is a bug where certain custom settings from `use-package' is
;; not effective.
;;
;; One way is to add `use-package' into the `custom-enabled-themes' list and
;; make it persistent across sessions, but it may be accidentally disabled.
;;
;; The other way is to add following code at the end of startup.
;;
;; (let ((theme 'use-package))
;;   (enable-theme theme)
;;   (setq custom-enabled-themes (remq theme custom-enabled-themes)))

;;; Code:

(use-package evil
  :ensure t
  :demand t
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

  :config
;;; State hacks
  ;; This is to implement a more flexible state dispatcher other than using Evil
  ;; built-in matchers.

  (defvar pew::evil::initial-state-plist
    `( :override nil
       :minor ((view-mode . motion))
       :major ((messages-buffer-mode . motion)
               (help-mode . motion)
               (image-mode . motion)
               (view-mode . motion)
               (Man-mode . motion)
               (woman-mode . motion))
       :name ((,(pewlib::workspace::map-buffer-regex '(:scratch :edit-indirect :org-src :org-export) 'concat) . normal)
              (,(pewlib::workspace::map-buffer-regex '(:eldoc :tree-sitter-explorer :org-babel) 'concat) . motion)) )
    "A plist to determine buffer initial state by different conditions.
Each property should have the following values.  The precedence is from highest
to lowest.
Whenever a rule matches, the evaluation is stopped and the buffer Evil state
will change to that evaluated state.
(:override (FUNC ...))
(:minor ((BUFFER-MINOR-MODE . EVIL-STATE) ...))
(:major ((BUFFER-MAJOR-MODE . EVIL-STATE) ...))
(:name ((REGEX . EVIL-STATE) ...))

':override': A list of functions which are invoked under current buffer context
one by one.  The function should return a valid Evil state symbol, like 'normal.
This enables more complex logic when determining buffer initial state.
':minor': A list of cons that maps buffer minor mode to Evil state.
':major': A list of cons that maps buffer major mode to Evil state.
':name': A list of cons that matches buffer name with regex in car with th
Evil state in cdr.")

  (evil-define-state pewinit
    "A dummy state used to determine buffer initial Evil state.
This dummy state means to be an intermidiate state which transits to another
legit Evil state immediately upon different conditions."
    :tag "<PEW>"
    :message "-- PEWINIT --"
    (if (evil-pewinit-state-p)
        (add-hook 'post-command-hook #'pew::evil::dispatch-initial-state nil t)
      (remove-hook 'post-command-hook #'pew::evil::dispatch-initial-state t)))

  (defun pew::evil::dispatch-initial-state (&optional command)
    "Advice to alter `evil-pewinitial-state' toggle behavior.  This advice works
in conjunction with the toggle to decide a buffer's initial Evil state.
This is an advanced method to determine initial state rather than using
`evil-set-initial-state' and `evil-buffer-regexps'."
    (if (evil-pewinit-state-p)
        (let ((state (or
                      ;; Check override
                      (save-excursion (named-let check ((list (plist-get pew::evil::initial-state-plist :override)))
                                        (let ((f (car list)))
                                          (if f (or (funcall f) (check (cdr list)))))))
                      ;; Check minor mode
                      ;; TODO: Potentially bugged due to the delay of the minor mode variable setting.
                      (cdr (seq-find (lambda (cons) (symbol-value (car cons)))
                                     (plist-get pew::evil::initial-state-plist :minor)))
                      ;; Check major mode
                      (cdr (seq-find (lambda (cons) (eq major-mode (car cons)))
                                     (plist-get pew::evil::initial-state-plist :major)))
                      ;; Check buffer name
                      (cdr (seq-find (lambda (cons) (string-match-p (car cons) (buffer-name)))
                                     (plist-get pew::evil::initial-state-plist :name))))))
          (cond
           ;; Matched by rules
           (state
            (evil-change-state state))
           ;; Editable buffer
           ((and (derived-mode-p 'prog-mode 'text-mode 'fundamental-mode)
                 ;; Actual file buffer or unsaved editable buffer
                 (or (buffer-file-name)
                     (not buffer-read-only)))
            (evil-change-state 'normal))
           ;; Use Emacs default key bindings otherwise
           (t
            (evil-change-state 'emacs))))))

  ;; Clear Evil built-in rules
  (setq evil-default-state 'pewinit
        evil-emacs-state-modes nil
        evil-motion-state-modes nil
        evil-normal-state-modes nil
        evil-insert-state-modes nil
        evil-visual-state-modes nil
        evil-replace-state-modes nil
        evil-operator-state-modes nil
        evil-buffer-regexps nil)

  ;; State tags
  (setq evil-emacs-state-tag         "[EM]"
        evil-normal-state-tag        "[NO]"
        evil-insert-state-tag        "[IN]"
        evil-replace-state-tag       "[RE]"
        evil-visual-char-tag         "[VI]"
        evil-visual-line-tag         "[VL]"
        evil-visual-block-tag        "[VB]"
        evil-visual-screen-line-tag  "[VS]"
        evil-motion-state-tag        "[MO]"
        evil-operator-state-tag      "[..]")

;;; Utility functions
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
    (let ((bindings (mapcan (if leader
                                (lambda (x) (list (kbd (concat "<leader>" (car x))) (cdr x)))
                              (lambda (x) (list (kbd (car x)) (cdr x))))
                            bindings)))
      (mapc (lambda (m) (apply 'evil-define-key* state m bindings))
            (if (listp map) map (list map)))))

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
    (setq evil-ex-search-pattern (evil-ex-make-pattern (pew::evil::escape-pattern (buffer-substring-no-properties beg end))
                                                       'sensitive
                                                       t))
    (evil-yank beg end)
    (ignore-error 'search-failed
      (evil-ex-search-next)))

  (defun pew::evil::visual-search-region-text ()
    "Search the text selected in visual state."
    (interactive)
    (when (evil-visual-state-p)
      (setq evil-ex-search-count 1
            evil-ex-search-direction 'forward)
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

  (defun pew::evil::search-word ()
    "Search and highlight the word under cursor but don't jumpt to the next."
    (interactive)
    (evil-ex-search-word-forward)
    (evil-ex-search-previous))

;;; Keybindings
  ;; Toggle key
  (evil-set-toggle-key "C-x m")
  (global-set-key (kbd "C-x C-m") #'evil-mode)

  ;; Leader keys
  (evil-set-leader '(normal motion visual) (kbd "RET")) ;; <leader>
  (evil-set-leader '(normal motion visual) (kbd "DEL") :localleader) ;; <localleader>

  ;; Normal and motion state bindings with leader key
  (pew::evil::set-key '(normal motion visual) 'global :leader
    ;; Search and substitution
    '(("cs" . pew::evil::replace-last-search)))

  ;; Command state bindings
  (pew::evil::set-key '(normal motion visual) 'global nil
    '(("SPC" . pewkey-map)
      ;; Search
      ("#" . evil-ex-nohighlight)
      ;; Default jump forward is C-i which is bad in terminal environment
      ("C-M-o" . evil-jump-forward)))

  ;; Normal and motion state specific
  (pew::evil::set-key '(normal motion) 'global nil
    '(("*" . pew::evil::search-word)))

  ;; Visual state specific
  (pew::evil::set-key 'visual 'global nil
    ;; Search
    '(("*" . pew::evil::visual-search-region-text)))

  ;; Elisp with leader
  (pewcfg :eval-after
          (elisp-mode
           (pew::evil::set-key '(normal motion visual) (list emacs-lisp-mode-map lisp-interaction-mode-map) :leader
             ;; Quick eval
             '(("eb" . eval-buffer)
               ("er" . eval-region)
               ("ef" . eval-defun)
               ("ee" . eval-last-sexp)))))

;;; Workaround
  ;; Evil X settings
  ;; Don't allow Evil to kill selected region when yanking
  ;; See: https://emacs.stackexchange.com/questions/14940/evil-mode-visual-selection-copies-text-to-clipboard-automatically/15054#15054
  (define-advice evil-visual-update-x-selection (:override (&rest _args) pew::evil::visual-update-x-selection))

;;; Enable Evil mode last to ensure most of the settings work
  (evil-mode 1)) ;; End evil

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(provide 'elpa-evil)
;;; elpa-evil.el ends here
