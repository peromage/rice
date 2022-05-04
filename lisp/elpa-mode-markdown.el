;;; elpa-mode-markdown.el --- Markdown mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew/markdown-mode/setup ()
  "My markdown mode hook."
  (setq line-move-visual t)
  (visual-line-mode))

(use-package markdown-mode
  :hook (markdown-mode . pew/markdown-mode/setup))

(provide 'elpa-mode-markdown)
;;; elpa-mode-markdown.el ends here
