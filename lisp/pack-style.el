;;; pack-style.el --- Visual enhancement -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Colors schemes
(use-package moe-theme :defer t)
(use-package doom-themes :defer t)
(use-package dracula-theme :defer t)

(load-theme 'dracula t)

;; Icons
(use-package all-the-icons)

;; Modeline
(use-package doom-modeline
  ;; https://github.com/seagle0128/doom-modeline/issues/187#issuecomment-508973014
  :custom-face
  (mode-line ((t (:height 0.9))))
  (mode-line-inactive ((t (:height 0.9))))
  :init
  (setq doom-modeline-height 1)
  (doom-modeline-mode 1))

(provide 'pack-style)
;;; pack-style.el ends here
