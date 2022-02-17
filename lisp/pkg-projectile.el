;;; pkg-projectile.el --- Project management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Projectile
;;------------------------------------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t)
  (projectile-mode 1))

;;------------------------------------------------------------------------------
;; Framework integration
;;------------------------------------------------------------------------------

;; Use if guard to prevent automatic installation when the framework doesn't exist

;; Helm integration
(if (featurep 'helm)
    (use-package helm-projectile
      :after helm
      :config
      (setq projectile-completion-system 'helm)))

;; Ivy integration
(if (featurep 'ivy)
    (use-package counsel-projectile
      :requires ivy
      :config
      (setq projectile-completion-system 'ivy)
      (counsel-projectile-mode 1)))

(provide 'pkg-projectile)
;;; pkg-projectile.el ends here
