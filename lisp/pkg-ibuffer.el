;;; pkg-ibuffer.el --- Builtin buffer manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ibuffer
  :commands (ibuffer list-buffers)
  :init
  (defalias 'list-buffers 'ibuffer))

(provide 'pkg-ibuffer)
;;; pkg-ibuffer.el ends here
