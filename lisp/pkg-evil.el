;;; pkg-evil.el --- Vim layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Functions and variables
;;------------------------------------------------------------------------------

(defun pew/evil/global-set-key (state binding-list)
  "Set a list of keybindings BINDING-LIST to a STATE globally."
  (dolist (binding binding-list)
    (evil-global-set-key state (kbd (car binding)) (cdr binding))))


(defun pew/evil/set-initial-state (mode-state-list)
  "Set the initial state for a mode defined in the alist MODE-STATE-LIST."
  (dolist (mode-state mode-state-list)
    (evil-set-initial-state (car mode-state) (cdr mode-state))))

(defun pew/evil/close-window ()
  "Close window on conditional.  If there is only one window then close the tab."
  (interactive)
  (cond ((one-window-p)
         (tab-bar-close-tab)
         (previous-window))
        (t (delete-window))))

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

;;------------------------------------------------------------------------------
;; Evil setup
;;------------------------------------------------------------------------------

(use-package evil
  :config
  (setq evil-want-integration t
        evil-want-keybinding t
        evil-want-minibuffer nil
        evil-disable-insert-state-bindings t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-auto-balance-windows t
        evil-ex-search-highlight-all t
        evil-ex-search-persistent-highlight t
        evil-symbol-word-search nil
        evil-kill-on-visual-paste t
        evil-search-module 'evil-search)

  (evil-mode 1)

  ;; Key bindings in normal and motion state
  (evil-set-leader '(normal motion) (kbd "SPC"))
  (let ((normal-bindings
         '(("<leader>w" . save-buffer)
           ("<leader>q" . pew/evil/close-window)
           ("<leader>h" . evil-window-left)
           ("<leader>j" . evil-window-down)
           ("<leader>k" . evil-window-up)
           ("<leader>l" . evil-window-right)
           ("<leader>s" . evil-window-split)
           ("<leader>v" . evil-window-vsplit)
           ("<leader>t" . tab-bar-new-tab)
           ("<leader>f" . tab-bar-switch-to-next-tab)
           ("<leader>b" . tab-bar-switch-to-prev-tab)
           ("<leader>n" . next-buffer)
           ("<leader>p" . previous-buffer)
           ("<leader>g" . pew/show-file-path)
           ("<leader>cs" . pew/evil/replace-last-search)
           ("<left>" . evil-window-decrease-width)
           ("<down>" . evil-window-decrease-height)
           ("<up>" . evil-window-increase-height)
           ("<right>" . evil-window-increase-width)
           ("#" . evil-ex-nohighlight))))
    (pew/evil/global-set-key 'normal normal-bindings)
    (pew/evil/global-set-key 'motion normal-bindings))

  ;; Key bindings in visual state
  (let ((visual-bindings
         '(("*" . pew/evil/visual-search-selected))))
    (pew/evil/global-set-key 'visual visual-bindings))

  ;; Explicitly set the initial state for a mode
  ;; States are: emacs, motion, normal, insert, visual
  (let ((initial-states
         '((help-mode . motion)
           (tab-switcher-mode . emacs)
           (xref--xref-buffer-mode . emacs)
           (flycheck-error-list-mode . emacs)
           (ivy-occur-grep-mode . emacs)
           (dired-mode . emacs)
           )))
    (pew/evil/set-initial-state initial-states)))

;;------------------------------------------------------------------------------
;; Evil enhancement
;;------------------------------------------------------------------------------

;;(use-package evil-collection
;;  :config
;;  (setq evil-want-keybinding nil)
;;  (evil-collection-init))

;; Make Evil undo/redo easier
(use-package undo-tree
  :hook (evil-local-mode . turn-on-undo-tree-mode)
  :config
  (evil-set-undo-system 'undo-tree))

(provide 'pkg-evil)
;;; pkg-evil.el ends here
