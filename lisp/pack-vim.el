;;; pack-vim.el --- Vim layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew-pack/evil-set-keybindings (state binding-list)
  "Set a list of keybindings BINDING-LIST to a STATE globally."
  (dolist (binding binding-list)
    (evil-global-set-key state (kbd (car binding)) (cdr binding))))

(defun pew-pack/evil-set-keybindings-in-normal-and-motion-state (binding-list)
  "Set BINDING-LIST in both normal and motion state."
  (pew-pack/evil-set-keybindings 'normal binding-list)
  (pew-pack/evil-set-keybindings 'motion binding-list))

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
        evil-kill-on-visual-paste t
        evil-search-module 'evil-search)
  (evil-mode 1)
  :config
  ;; Leader key bindings in normal mode
  (evil-set-leader '(normal motion) (kbd "SPC"))
  (let ((keybindings
         '(("<leader>w" . save-buffer)
           ("<leader>q" . evil-quit)
           ("<leader>h" . evil-window-left)
           ("<leader>j" . evil-window-down)
           ("<leader>k" . evil-window-up)
           ("<leader>l" . evil-window-right)
           ("<leader>s" . evil-window-split)
           ("<leader>v" . evil-window-vsplit))))
    (pew-pack/evil-set-keybindings-in-normal-and-motion-state keybindings))
  ;; Individual keys in normal mode
  (let ((keybindings
         '(("<left>" . evil-window-decrease-width)
           ("<down>" . evil-window-decrease-height)
           ("<up>" . evil-window-increase-height)
           ("<right>" . evil-window-increase-width))))
    (pew-pack/evil-set-keybindings-in-normal-and-motion-state keybindings))
  ;; Modes that don't use Evil
  (let ((excluded-modes
         '(flycheck-error-list-mode
           ivy-occur-grep-mode
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
