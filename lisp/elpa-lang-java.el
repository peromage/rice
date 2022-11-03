;;; elpa-lang-java.el --- Java mode -*- lexical-binding: t -*-

;;; Commentary:
;; Java major mode configuration.

;;; Code:

(use-package lsp-java
  :init
  (defun pew/java-mode/setup ()
    "Java mode setup."
    (lsp-deferred))

  :hook (java-mode . pew/java-mode/setup))

(provide 'elpa-lang-java)
;;; elpa-lang-java.el ends here
