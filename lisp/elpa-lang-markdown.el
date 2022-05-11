;;; elpa-lang-markdown.el --- Markdown mode -*- lexical-binding: t -*-
;;; Commentary:

;; When writting in `markdown-mode' some configurations are slightly different than coding modes.

;;; Code:

(use-package markdown-mode
  :init
  (defun pew/markdown-mode/setup ()
    "My markdown mode hook."
    (setq line-move-visual t)
    (visual-line-mode))

  :hook (markdown-mode . pew/markdown-mode/setup))

(provide 'elpa-lang-markdown)
;;; elpa-lang-markdown.el ends here
