;;; pack-git.el --- Git porcelain -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; Ediff
;;==============================================================================

(use-package ediff
  :ensure nil
  :defer t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-vertically))

;;==============================================================================
;; Magit
;;==============================================================================

(use-package magit
  :commands magit-status)

;;==============================================================================
;; Display changes besides the row number
;;==============================================================================

(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (setq git-gutter:modified-sign "**"
        git-gutter:added-sign "++"
        git-gutter:deleted-sign "--"
        git-gutter:unchanged-sign nil
        git-gutter:separator-sign nil
        git-gutter:update-interval 2
        git-gutter:visual-line nil
        git-gutter:hide-gutter nil
        git-gutter:verbosity 0)
  :config
  (global-git-gutter-mode 1)
  (set-face-foreground 'git-gutter:modified "yellow")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red")
  (set-face-foreground 'git-gutter:unchanged "unspecified")
  (set-face-foreground 'git-gutter:separator "unspecified")
  (set-face-background 'git-gutter:modified "unspecified")
  (set-face-background 'git-gutter:added "unspecified")
  (set-face-background 'git-gutter:deleted "unspecified")
  (set-face-background 'git-gutter:unchanged "unspecified")
  (set-face-background 'git-gutter:separator "unspecified"))

(provide 'pack-git)
;;; pack-git.el ends here
