;;; pkg-themes.el --- Visual enhancement -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Colors schemes
(use-package doom-themes :defer t)
(use-package dracula-theme :defer t)
(use-package moe-theme :defer t)
(use-package modus-themes :defer t)

;; Icons
(use-package all-the-icons :defer t)

;; Modeline
(use-package doom-modeline
  ;; https://github.com/seagle0128/doom-modeline/issues/187#issuecomment-508973014
  :custom-face
  (mode-line ((t (:height 0.9))))
  (mode-line-inactive ((t (:height 0.9))))
  :custom
  (doom-modeline-height 1)
  :config
  (doom-modeline-mode 1))

;; At last, enables global color theme with some addtional settings
(pew/load-theme 'doom-dracula)

(provide 'pkg-themes)
;;; pkg-themes.el ends here
