;;; pack-theme.el --- Visual enhancement -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew/disable-all-themes ()
  "Disable all themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

;; Colors schemes
(use-package moe-theme :defer t)
(use-package doom-themes :defer t)
(use-package dracula-theme :defer t)

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

;; At last, enables global color theme with some addtional settings
(pew/disable-all-themes)
(load-theme 'doom-dracula t)
(set-face-attribute 'tab-bar nil :inherit 'default)

(provide 'pack-theme)
;;; pack-theme.el ends here
