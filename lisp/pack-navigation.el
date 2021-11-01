;;; pack-navigation.el --- Navigation enhancement -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Built-in Dired
;;------------------------------------------------------------------------------

(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-find-file)
  :bind (("C-x C-d" . (lambda () (interactive) (find-file default-directory)))
         :map dired-mode-map
         ("RET" . dired-find-alternate-file)
         ("DEL" . (lambda () (interactive) (find-alternate-file ".."))))
  :init
  (setq dired-listing-switches "-alFD --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top)
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (use-package dired-x :ensure nil))

;;------------------------------------------------------------------------------
;; Sidebar file navigation
;;------------------------------------------------------------------------------

;;(use-package treemacs)

;;------------------------------------------------------------------------------
;; Project management
;;------------------------------------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode 1))

;;------------------------------------------------------------------------------
;; Key binding quick view
;;------------------------------------------------------------------------------

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-popup-type 'minibuffer
        which-key-show-early-on-C-h nil
        which-key-idle-delay 1.0)
  :config
  (which-key-mode 1)
  (which-key-setup-minibuffer))

;;------------------------------------------------------------------------------
;; Layout management
;;------------------------------------------------------------------------------

;;(use-package eyebrowse
;;  :commands eyebrowse-mode)
;;
;;(use-package edwina
;;  ;; Enabled on demand
;;  :commands (edwina-mode edwin-mode)
;;  :config
;;  ;;(edwina-setup-dwm-keys)
;;  ;;(edwina-mode 1)
;;  )

(provide 'pack-navigation)
;;; pack-navigation.el ends here
