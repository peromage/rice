;;; pack-helm.el --- Helm for quick search -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package helm
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini))
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (setq helm-split-window-inside-p t
        helm-echo-input-in-header-line t
        helm-ff-file-name-history-use-recentf t
        helm-autoresize-max-height 30
        helm-autoresize-min-height 20
        helm-M-x-fuzzy-match t))

(provide 'pack-helm)
;;; pack-helm.el ends here
