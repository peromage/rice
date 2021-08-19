;;; pack-helm.el --- Helm for quick search -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package helm
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list))
  :config
  (helm-mode 1))

(provide 'pack-helm)
;;; pack-helm.el ends here
