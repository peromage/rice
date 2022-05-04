;;; pkg-flycheck.el --- Syntax checker -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode 1))

(provide 'pkg-flycheck)
;;; pkg-flycheck.el ends here
