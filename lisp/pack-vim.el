;;; pack-vim.el --- Vim layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-minibuffer nil
        evil-disable-insert-state-bindings t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-auto-balance-windows t
        evil-search-module 'evil-search)
  (evil-mode 1)
  :config
  ;; Leader key bindings in normal mode
  (evil-set-leader 'normal (kbd "SPC"))
  (let ((key-bindings-with-leader-in-normal
         '(("w" . save-buffer)
           ("q" . evil-quit)
           ("h" . evil-window-left)
           ("j" . evil-window-down)
           ("k" . evil-window-up)
           ("l" . evil-window-right)
           ("s" . evil-window-split)
           ("v" . evil-window-vsplit))))
    (dolist (binding key-bindings-with-leader-in-normal)
      (evil-global-set-key
       'normal
       (kbd (format "<leader>%s" (car binding)))
       (cdr binding))))
  ;; Individual keys in normal mode
  (let ((key-bindings-in-normal
         '(("<left>" . evil-window-decrease-width)
           ("<down>" . evil-window-decrease-height)
           ("<up>" . evil-window-increase-height)
           ("<right>" . evil-window-increase-width))))
    (dolist (binding key-bindings-in-normal)
      (evil-global-set-key
       'normal
       (kbd (car binding))
       (cdr binding))))
  ;; Modes that don't use Evil
  (let ((excluded-modes
         '(dired-mode
           ;;help-mode
           ;;magit-status-mode
           )))
    (dolist (mode excluded-modes)
      (evil-set-initial-state mode 'emacs))))

(provide 'pack-vim)
;;; pack-vim.el ends here
