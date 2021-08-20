;;; pack-navi.el --- Navigation enhancement -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; Completion framework
;;==============================================================================

;; Contains ivy, counsel and swipper
;; See: https://oremacs.com/swiper/#installing-from-emacs-package-manager
(use-package counsel
  :bind-keymap
  ("C-c c" . counsel-mode-map)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1))

;; (use-package helm
;;   :diminish helm-mode
;;   :bind (("M-x" . helm-M-x)
;;          ("C-x C-f" . helm-find-files)
;;          ("C-x C-b" . helm-buffers-list)
;;          ("C-x b" . helm-mini))
;;   :config
;;   (helm-mode 1)
;;   (helm-autoresize-mode 1)
;;   (setq helm-split-window-inside-p t
;;         helm-echo-input-in-header-line t
;;         helm-ff-file-name-history-use-recentf t
;;         helm-autoresize-max-height 30
;;         helm-autoresize-min-height 20
;;         helm-M-x-fuzzy-match t))

;;==============================================================================
;; File management
;;==============================================================================

(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-find-file)
  :bind (("C-x C-d" . (lambda () (interactive) (find-file default-directory)))
         :map dired-mode-map
         ("RET" . dired-find-alternate-file)
         ("^" . (lambda () (interactive) (find-alternate-file "..")))
         ("DEL" . (lambda () (interactive) (find-alternate-file ".."))))
  :init
  (setq dired-listing-switches "-alFD --group-directories-first")
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (use-package dired-x
    :ensure nil))

;;==============================================================================
;; Project management
;;==============================================================================

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode 1))

;;==============================================================================
;; Key binding quick view
;;==============================================================================

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(provide 'pack-navi)
;;; pack-navi.el ends here
