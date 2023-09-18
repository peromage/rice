;;; elpa-lang-markdown.el --- markdown mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: markdown-mode
(pewcfg::use-package markdown-mode
  :hook (markdown-mode . pew::markdown-mode::oninit)
  :config
  (defun pew::markdown-mode::oninit ()
    "`markdown-mode' initialization."
    (pew::text-mode-oninit)))

(provide 'elpa-lang-markdown)
;;; elpa-lang-markdown.el ends here
