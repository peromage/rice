;;; elpa-helm.el --- Framework Helm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; Helm core package
;;------------------------------------------------------------------------------

(use-package helm
  :demand t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-s" . helm-occur)
         ("C-x f" . helm-find)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini))
  :custom
  (helm-split-window-inside-p t)
  (helm-echo-input-in-header-line t)
  (helm-use-frame-when-more-than-two-windows nil)
  (helm-always-two-windows nil)
  (helm-ff-file-name-history-use-recentf t)
  (helm-autoresize-max-height 30)
  (helm-autoresize-min-height 20)
  (helm-M-x-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-ff-fuzzy-matching t)
  (helm-apropos-fuzzy-match t)
  (helm-recentf-fuzzy-match t)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1))

(provide 'elpa-helm)
;;; elpa-helm.el ends here
