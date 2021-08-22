;;; pack-style.el --- Visual enhancement -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Colors
(use-package moe-theme :defer t)
(use-package doom-themes :defer t)
(use-package dracula-theme :init (load-theme 'dracula t))

;; Icons
(use-package all-the-icons)

;; Modeline
(use-package doom-modeline :defer t)

(provide 'pack-style)
;;; pack-style.el ends here
