;;; base-ediff.el --- Builtin diff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-vertically))

(provide 'base-ediff)
;;; base-ediff.el ends here
