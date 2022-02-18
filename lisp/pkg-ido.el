;;; pkg-ido.el --- Ido mode related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ido
  :ensure nil
  :config
  (setq ido-enable-flex-matching t
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-save-directory-list-file (expand-file-name "ido.last" pew/temp-dir))

  (ido-mode 1))

(provide 'pkg-ido)
;;; pkg-ido.el ends here
