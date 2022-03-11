;;; pkg-ido.el --- Ido mode related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ido
  :custom
  (ido-enable-flex-matching t)
  (ido-default-file-method 'selected-window)
  (ido-default-buffer-method 'selected-window)
  :config
  (ido-mode 1))

(provide 'pkg-ido)
;;; pkg-ido.el ends here
