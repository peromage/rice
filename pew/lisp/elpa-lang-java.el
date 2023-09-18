;;; elpa-lang-java.el --- java mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: lsp-java
(pewcfg::use-package lsp-java
  :hook (java-mode . pew::java-mode::oninit)
  :config
  (defun pew::java-mode::oninit ()
    "`java-mode' initialization."
    (lsp-deferred)))

(provide 'elpa-lang-java)
;;; elpa-lang-java.el ends here
