;;; elpa-lang-markdown.el --- Markdown mode -*- lexical-binding: t -*-

;;; Commentary:
;; Markdown major mode configuration.

;;; Code:

(use-package markdown-mode
  :init
  (defun pew/markdown-mode/setup ()
    "Markdown mode steup."
    (pew/text-common-setup))

  :hook (markdown-mode . pew/markdown-mode/setup))

(provide 'elpa-lang-markdown)
;;; elpa-lang-markdown.el ends here
