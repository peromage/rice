;;; pack-vim.el --- Vim layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; Functions and variables
;;==============================================================================

(defun pew-evil/global-set-key (state binding-list)
  "Set a list of keybindings BINDING-LIST to a STATE globally."
  (dolist (binding binding-list)
    (evil-global-set-key state (kbd (car binding)) (cdr binding))))

(defun pew-evil/close-window ()
  "Close window on conditional.  If there is only one window then close the tab."
  (interactive)
  (cond ((one-window-p)
         (tab-bar-close-tab)
         (previous-window))
        (t (delete-window))))

(defun pew-evil/escape-region ()
  "Return selected region with regex special character escaped."
  (if (use-region-p)
      (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (/= (length selection) 0)
            (regexp-quote selection)
          nil))
    nil))

(defun pew-evil/search-region ()
  "Use / command to search the selected region."
  (interactive)
  (let ((content (pew-evil/escape-region)))
    (when content
      (setq evil-ex-search-pattern (list content t t))
      (setq evil-ex-search-direction 'forward)
      ;; Either use `evil-ex-search' or `evil-ex-search-next' the cursor jumps to the
      ;; next one anyways despite the `evil-ex-search-direction'.
      ;; Also `evil-ex-search' doesn't recognize 0.
      ;; This implementation is ugly. Hope they can improve the function in the
      ;; future.
      (evil-ex-search-next)
      (evil-ex-search-previous))))

(defun pew-evil/visual-search-region ()
  "Search the selected region visual state and return to normal state."
  (interactive)
  (when (evil-visual-state-p)
    (pew-evil/search-region)
    (evil-normal-state)))

;;==============================================================================
;; Setup
;;==============================================================================

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding t
        evil-want-minibuffer nil
        evil-disable-insert-state-bindings t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-auto-balance-windows t
        evil-ex-search-highlight-all t
        evil-ex-search-persistent-highlight t
        evil-symbol-word-search t
        evil-kill-on-visual-paste t
        evil-search-module 'evil-search)
  (evil-mode 1)
  :config
  ;; Key bindings in normal and motion state
  (evil-set-leader '(normal motion) (kbd "SPC"))
  (let ((normal-bindings
         '(("<leader>w" . save-buffer)
           ("<leader>q" . pew-evil/close-window)
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
           ("<left>" . evil-window-decrease-width)
           ("<down>" . evil-window-decrease-height)
           ("<up>" . evil-window-increase-height)
           ("<right>" . evil-window-increase-width)
           ("#" . evil-ex-nohighlight))))
    (pew-evil/global-set-key 'normal normal-bindings)
    (pew-evil/global-set-key 'motion normal-bindings))
  ;; Key bindings in visual state
  (let ((visual-bindings
         '(("*" . pew-evil/visual-search-region))))
    (pew-evil/global-set-key 'visual visual-bindings))
  ;; Modes that don't use Evil
  (let ((excluded-modes
         '(flycheck-error-list-mode
           ivy-occur-grep-mode
           tab-switcher-mode
           xref--xref-buffer-mode
           ;;dired-mode
           ;;magit-status-mode
           )))
    (dolist (mode excluded-modes)
      (evil-set-initial-state mode 'emacs))))

;;(use-package evil-collection
;;  :after evil
;;  :config
;;  (setq evil-want-keybinding nil)
;;  (evil-collection-init))

(provide 'pack-vim)
;;; pack-vim.el ends here
