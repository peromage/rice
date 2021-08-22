;;; pack-navi-helm.el --- Framework Helm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package helm
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x f" . helm-find-files)
         ("C-x C-f" . helm-find)
         ("C-x C-b" . helm-mini)
         ("C-x b" . helm-buffers-list))
  :init
  (setq helm-split-window-inside-p t
        helm-echo-input-in-header-line t
        helm-ff-file-name-history-use-recentf t
        helm-autoresize-max-height 30
        helm-autoresize-min-height 20
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-ff-fuzzy-matching t
        helm-apropos-fuzzy-match t
        helm-recentf-fuzzy-match t)
  (helm-mode 1)
  :config
  (helm-autoresize-mode 1))

(provide 'pack-navi-helm)
;;; pack-navi-helm.el ends here
