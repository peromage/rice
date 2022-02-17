;;; pkg-ediff.el --- Builtin diff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ediff
  :ensure nil
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-vertically))

(provide 'pkg-ediff)
;;; pkg-ediff.el ends here
