;;; elpa-lang-markdown.el --- Markdown mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown major mode configuration.

;;; Code:
(use-package markdown-mode
  :hook (markdown-mode . pew::markdown-mode::oninit)

  :config
  (defun pew::markdown-mode::oninit ()
    "`markdown-mode' initialization."
    (pew::text-mode-oninit)))

(provide 'elpa-lang-markdown)
;;; elpa-lang-markdown.el ends here
