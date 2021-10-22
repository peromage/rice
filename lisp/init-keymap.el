;;; init-keymap.el --- Keymaps for original features -*- lexical-binding: t -*-
;;; Commentary:

;; Global keymaps respect vanilla Emacs settings

;;; Code:

;;------------------------------------------------------------------------------
;; Shared functions
;;------------------------------------------------------------------------------

(defun pew/global-set-key (keybindings)
  "Globally bind keys defined in the alist KEYBINDINGS.
The alist KEYBINDINGS should be something like:
  '((\"key strokes\" . command)
    ([key strokes] . command))"
  (dolist (binding keybindings)
    (let ((keys (car binding))
          (cmd (cdr binding)))
      (if (vectorp keys)
          (global-set-key keys cmd)
        (global-set-key (kbd keys) cmd)))))

;;------------------------------------------------------------------------------
;; Keymaps
;;------------------------------------------------------------------------------

(let ((global-keys
       '(("C-x t SPC" . tab-bar-select-tab-by-name)
         ("C-x t f" . tab-bar-switch-to-next-tab)
         ("C-x t b" . tab-bar-switch-to-prev-tab)
         ("C-x t >" . (lambda () (interactive) (tab-bar-move-tab 1)))
         ("C-x t <" . (lambda () (interactive) (tab-bar-move-tab -1)))
         ("C-x t t" . tab-bar-new-tab)
         ("C-x t l" . tab-switcher)
         ([remap next-buffer] . pew/next-buffer)
         ([remap previous-buffer] . pew/prev-buffer))))
  (pew/global-set-key global-keys))

(provide 'init-keymap)
;;; init-keymap.el ends here
