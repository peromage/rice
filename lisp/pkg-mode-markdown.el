;;; pkg-mode-markdown.el --- Markdown mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pew/markdown-mode/setup ()
  "My markdown mode hook."
  (setq line-move-visual t)
  (visual-line-mode))

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . pew/markdown-mode/setup))

(provide 'pkg-mode-markdown)
;;; pkg-mode-markdown.el ends here
