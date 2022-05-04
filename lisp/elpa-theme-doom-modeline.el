;;; elpa-theme-doom-modeline.el --- Modeline from Doom -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package doom-modeline
  ;; https://github.com/seagle0128/doom-modeline/issues/187#issuecomment-508973014
  :custom-face
  (mode-line ((t (:height 0.9))))
  (mode-line-inactive ((t (:height 0.9))))
  :custom
  (doom-modeline-height 1)
  :config
  (doom-modeline-mode 1))

(provide 'elpa-theme-doom-modeline)
;;; elpa-theme-doom-modeline.el ends here
