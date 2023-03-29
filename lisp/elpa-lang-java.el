;;; elpa-lang-java.el --- Java mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Java major mode configuration.

;;; Code:
(use-package lsp-java
  :hook (java-mode . pew/java-mode/on-init)

  :config
  (defun pew/java-mode/on-init ()
    "`java-mode' initialization."
    (lsp-deferred)))

(provide 'elpa-lang-java)
;;; elpa-lang-java.el ends here
