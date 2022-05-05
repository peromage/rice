;;; elpa-git.el --- Git packages -*- lexical-binding: t -*-
;;; Commentary:

;; This module contains git related packages and configurations

;;; Code:

;;;; Frontend

;; Magit is a powerful git frontend.
;; No special configuration needed usually.
(use-package magit
  :commands magit-status)

;;;; Status

;; `git-gutter-mode' displays line changes on the left margin.
(use-package git-gutter
  :diminish git-gutter-mode
  :custom
  (git-gutter:modified-sign "**")
  (git-gutter:added-sign "++")
  (git-gutter:deleted-sign "--")
  (git-gutter:unchanged-sign nil)
  (git-gutter:separator-sign nil)
  (git-gutter:update-interval 2)
  (git-gutter:visual-line nil)
  (git-gutter:hide-gutter nil)
  (git-gutter:verbosity 0)
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

;;;; Diff tools

;; Builtin diff tool.
(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-vertically))

(provide 'elpa-git)
;;; elpa-git.el ends here
