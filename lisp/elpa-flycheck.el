;;; elpa-flycheck.el --- Syntax checker -*- lexical-binding: t -*-
;;; Commentary:

;; Spell and syntax checker.  Helpful while coding.

;;; Code:

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode 1))

(provide 'elpa-flycheck)
;;; elpa-flycheck.el ends here
