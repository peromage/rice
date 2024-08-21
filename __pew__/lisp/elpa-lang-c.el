;;; elpa-lang-c.el --- c/c++ mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: cc-mode
(pewcfg::use-package cc-mode
  :defer t
  :hook ((c-mode . pew::cc-mode::oninit)
         (c++-mode . pew::cc-mode::oninit))

  :config
  ;; Setup functions
  (defun pew::cc-mode::oninit ()
    "Common CC mode setup."
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'innamespace 0)
    ;; Indentation
    (setq-local indent-tabs-mode nil
                c++-tab-always-indent t
                c-basic-offset 4
                c-indent-level 4
                tab-width 4
                tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
                c-syntactic-indentation t
                c-syntactic-indentation-in-macros t
                ;; Fill columns
                adaptive-fill-mode nil
                ;; Macro line continuation
                c-backslash-column 80
                c-backslash-max-column 160
                c-auto-align-backslashes t)))

(provide 'elpa-lang-c)
;;; elpa-lang-c.el ends here
