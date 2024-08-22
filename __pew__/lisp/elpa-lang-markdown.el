;;; elpa-lang-markdown.el --- markdown mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: markdown-mode
(pewcfg::use-package markdown-mode
  :defer t
  :hook (markdown-mode . pew::markdown-mode::oninit)
  :config
  (defun pew::markdown-mode::oninit ()
    "`markdown-mode' initialization."
    (pewlib::as-text-mode)))

(provide 'elpa-lang-markdown)
;;; elpa-lang-markdown.el ends here
