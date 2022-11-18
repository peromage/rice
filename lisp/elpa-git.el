;;; elpa-git.el --- Git packages -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains git related packages and configurations

;;; Code:

;;; Frontend
;; Magit is a powerful git frontend.
(use-package magit
  :commands magit-status

  :bind (("C-c g g" . magit-status)
         ("C-c g f" . magit-file-dispatch)
         ("C-c g d" . magit-dispatch)
         ("C-c g p" . magit-project-status))

  :custom
  ;; Don't use the default bindings under "C-x" prefix
  (magit-define-global-key-bindings nil))

;;; Status line
;; `git-gutter-mode' displays line changes on the left margin.
(use-package git-gutter
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

(provide 'elpa-git)
;;; elpa-git.el ends here
