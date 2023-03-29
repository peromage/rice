;;; elpa-lang-markdown.el --- Markdown mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown major mode configuration.

;;; Code:
(use-package markdown-mode
  :hook (markdown-mode . pew/markdown-mode/setup)

  :config
  (defun pew/markdown-mode/setup ()
    "Markdown mode steup."
    (pew/text-mode-on-init)))

(provide 'elpa-lang-markdown)
;;; elpa-lang-markdown.el ends here
