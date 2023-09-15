;;; elpa-git.el --- Git packages -*- lexical-binding: t; -*-

;;; Commentary:
;; This module contains git related packages and configurations

;;; Code:
;;; Frontend
;; Magit is a powerful git frontend.
(use-package magit
  :commands magit-status

  :bind (:map pew::M-u-map
         ("g" . magit-status)
         ("G" . magit-file-dispatch))

  :config
  (pewcfg
    :setq
    ;; Don't use the default bindings under "C-x" prefix
    (magit-define-global-key-bindings nil)))

;;; Status line
;; `git-gutter-mode' displays line changes on the left margin.
(use-package git-gutter
  :config
  (pewcfg
    :setq
    (git-gutter:modified-sign "**")
    (git-gutter:added-sign "++")
    (git-gutter:deleted-sign "--")
    (git-gutter:unchanged-sign nil)
    (git-gutter:separator-sign nil)
    (git-gutter:update-interval 2)
    (git-gutter:visual-line nil)
    (git-gutter:hide-gutter nil)
    (git-gutter:verbosity 0)
    :face
    (git-gutter:modified  :foreground "yellow"      :background "unspecified")
    (git-gutter:added     :foreground "green"       :background "unspecified")
    (git-gutter:deleted   :foreground "red"         :background "unspecified")
    (git-gutter:unchanged :foreground "unspecified" :background "unspecified")
    (git-gutter:separator :foreground "unspecified" :background "unspecified")
    :eval
    (global-git-gutter-mode 1)))

(provide 'elpa-git)
;;; elpa-git.el ends here
