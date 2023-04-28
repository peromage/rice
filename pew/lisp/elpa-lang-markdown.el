;;; elpa-lang-markdown.el --- Markdown mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown major mode configuration.

;;; Code:
(use-package markdown-mode
  :hook (markdown-mode . pew::markdown-mode::on-init)

  :config
  (defun pew::markdown-mode::on-init ()
    "`markdown-mode' initialization."
    (pew::text-mode-on-init)))

(provide 'elpa-lang-markdown)
;;; elpa-lang-markdown.el ends here
