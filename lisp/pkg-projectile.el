;;; pkg-projectile.el --- Project management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Projectile
;;------------------------------------------------------------------------------

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-enable-caching t)
  :config
  (projectile-mode 1))

;;------------------------------------------------------------------------------
;; Framework integration
;;------------------------------------------------------------------------------

;; Use if guard to prevent automatic installation when the framework doesn't exist

;; Helm integration
(if (featurep 'helm)
    (use-package helm-projectile
      :ensure t
      :after helm
      :custom
      (projectile-completion-system 'helm)))

;; Ivy integration
(if (featurep 'ivy)
    (use-package counsel-projectile
      :ensure t
      :after ivy
      :custom
      (projectile-completion-system 'ivy)
      :config
      (counsel-projectile-mode 1)))

(provide 'pkg-projectile)
;;; pkg-projectile.el ends here
