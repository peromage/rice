;;; base-electric.el --- Electric mode related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package electric
  :ensure nil
  :custom
   ;; No closing pair if the open pair precedes a non-whitespace character
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  :config
  (electric-pair-mode -1)
  (electric-indent-mode 1))

(provide 'base-electric)
;;; base-electric.el ends here
