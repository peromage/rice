;;; pkg-projectile.el --- Project management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t)
  (projectile-mode 1))

(provide 'pkg-projectile)
;;; pkg-projectile.el ends here
