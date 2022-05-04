;;; base-ido.el --- Ido mode -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for `ido-mode' in case I drop using any completion framework.

;;; Code:

(use-package ido
  :ensure nil
  :custom
  (ido-enable-flex-matching t)
  (ido-default-file-method 'selected-window)
  (ido-default-buffer-method 'selected-window)
  :config
  (ido-mode 1))

(provide 'base-ido)
;;; base-ido.el ends here
