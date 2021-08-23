;;; pack-navi.el --- Navigation enhancement -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; Completion framework
;;==============================================================================
(require 'pack-navi-ivy)

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
  (setq dired-listing-switches "-alFD --group-directories-first"
        dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (use-package dired-x :ensure nil))

;;==============================================================================
;; Project management
;;==============================================================================

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-known-projects-file (expand-file-name "cache/projectile-bookmarks.eld" user-emacs-directory)
        projectile-cache-file (expand-file-name "cache/projectile.cache" user-emacs-directory)
        projectile-enable-caching t)
  (projectile-mode 1)
  :config
  ;; Enhancement when Ivy or Helm present
  (cond ((featurep 'ivy)
         (setq projectile-completion-system 'ivy)
         (use-package counsel-projectile
           :init
           (counsel-projectile-mode 1)))
        ((featurep 'helm)
         (setq projectile-completion-system 'helm)
         (use-package helm-projectile))))

;;==============================================================================
;; Key binding quick view
;;==============================================================================

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-popup-type 'minibuffer
        which-key-show-early-on-C-h nil
        which-key-idle-delay 1.0)
  (which-key-mode 1)
  :config
  (which-key-setup-minibuffer))

;;==============================================================================
;; Layout management
;;==============================================================================

(use-package eyebrowse
  :init
  (eyebrowse-mode 1))

(provide 'pack-navi)
;;; pack-navi.el ends here
