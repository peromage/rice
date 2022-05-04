;;; base-ibuffer.el --- Builtin buffer manager -*- lexical-binding: t -*-
;;; Commentary:

;; A pretty decent buffer manager to replace the default shabby one.

;;; Code:

(use-package ibuffer
  :ensure nil
  :commands (ibuffer list-buffers)
  :init
  (defalias 'list-buffers 'ibuffer))

(provide 'base-ibuffer)
;;; base-ibuffer.el ends here
