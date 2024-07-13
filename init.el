;;; init.el --- pew bootstraps -*- lexical-binding: t; coding: utf-8; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;; Emacs version check
(let ((min-ver "28.1"))
  (if (version< emacs-version min-ver)
      (error "[pew] Emacs version %s+ is required" min-ver)))

(defvar pew::paths-plist
  (let* ((root (file-name-directory load-file-name))
         (topLevel (expand-file-name "__pew__" root)))
    (list
     :topLevel topLevel
     :lisp (expand-file-name "lisp" topLevel)
     :site-lisp (expand-file-name "site-lisp" topLevel)
     :pewcfg (expand-file-name "site-lisp/pewcfg" topLevel)
     :org-template (expand-file-name "org-templates" topLevel)
     :yas-template (expand-file-name "yasnippet-templates" topLevel)
     :custom (expand-file-name "custom.el" root)
     :org (expand-file-name "my-org-notes" root)))
  "Path definitions in this configuration.")

;;; Emacs variables
;; Configurations from the interactive `customize' interfaces.
(setq custom-file (plist-get pew::paths-plist :custom))
;; This config
(setq load-path (nconc (mapcar (lambda (k) (plist-get pew::paths-plist k))
                               '(:lisp :site-lisp :pewcfg))
                       load-path))

;;; Module loading
;; NOTE: The loading sequence is important
(require 'init-boot)
(require 'init-package)
(require 'pewcfg)
(require 'init-lib)
(require 'init-config)

;; Load ELPA packages
(require 'elpa-ui)
(require 'elpa-evil)
(require 'elpa-completion)
;; (require 'elpa-completion-ivy)
;; (require 'elpa-completion-company)
(require 'elpa-completion-vertico)
(require 'elpa-completion-corfu)
(require 'elpa-git)
(require 'elpa-org)
(require 'elpa-utils)

;; Language supports
(require 'elpa-lang)
(require 'elpa-lang-lsp) ;; LSP is the dependency of language modules
(require 'elpa-lang-c)
(require 'elpa-lang-csharp)
(require 'elpa-lang-java)
(require 'elpa-lang-lua)
(require 'elpa-lang-markdown)
(require 'elpa-lang-powershell)
(require 'elpa-lang-python)
(require 'elpa-lang-plantuml)
(require 'elpa-lang-mermaid)
(require 'elpa-lang-graphviz)

;; Load custom configuration which takes the highest precedence
(load custom-file :noerror)
(message "[pew] Normal init finished")

(provide 'init)
;;; init.el ends here
