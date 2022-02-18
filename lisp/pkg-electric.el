;;; pkg-electric.el --- Electric mode related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package electric
  :ensure nil
  :config
  (setq
   ;; No closing pair if the open pair precedes a non-whitespace character
   electric-pair-preserve-balance nil
   electric-pair-delete-adjacent-pairs nil)

  (electric-pair-mode 1)
  (electric-indent-mode 1))

(provide 'pkg-electric)
;;; pkg-electric.el ends here
