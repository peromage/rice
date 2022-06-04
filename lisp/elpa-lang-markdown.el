;;; elpa-lang-markdown.el --- Markdown mode -*- lexical-binding: t -*-
;;; Commentary:

;; When writting in `markdown-mode' some configurations are slightly different than coding modes.

;;; Code:

(use-package markdown-mode
  :hook (markdown-mode . pew/text-setup))

(provide 'elpa-lang-markdown)
;;; elpa-lang-markdown.el ends here
