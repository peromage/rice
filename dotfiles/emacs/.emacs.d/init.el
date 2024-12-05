;;; init.el --- pew bootstraps -*- lexical-binding: t; coding: utf-8; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;; Emacs version check
(let ((min-ver "29"))
  (if (version< emacs-version min-ver)
      (error "[pew] Emacs version %s+ is required" min-ver)))

(defconst pew::path-plist
  (let* ((root (file-name-directory load-file-name))
         (topLevel (expand-file-name "pew" root)))
    (list
     :topLevel topLevel
     :lisp (expand-file-name "lisp" topLevel)
     :site-lisp (expand-file-name "site-lisp" topLevel)
     :org-template (expand-file-name "org-templates" topLevel)
     :yas-template (expand-file-name "yasnippet" topLevel)
     :custom (expand-file-name "custom.el" root)
     :org (expand-file-name "my-org-notes" root)))
  "Path definitions in this configuration.")

;;; Emacs variables
;; Configurations from the interactive `customize' interfaces.
(setq custom-file (plist-get pew::path-plist :custom))
;; This config
(setq load-path (nconc (mapcar (lambda (k) (plist-get pew::path-plist k))
                               '(:lisp :site-lisp))
                       load-path))
(let ((default-directory (plist-get pew::path-plist :site-lisp)))
  (normal-top-level-add-subdirs-to-load-path))


;;; Module loading
;; NOTE: The loading sequence is important
(require 'pewcfg)
(require 'pewlib)
(require 'init-boot)
(require 'init-package)
(require 'init-config)

;; Load ELPA packages
(require 'elpa-ui)
(require 'elpa-evil)
(require 'elpa-completion-company)
;; (require 'elpa-completion-corfu)
;; (require 'elpa-minibuffer-ivy)
(require 'elpa-minibuffer-vertico)
(require 'elpa-utils)
(require 'elpa-org)
(require 'elpa-copilot)

;; Language supports
(require 'elpa-langs)
(require 'elpa-treesit)
;; (require 'elpa-lsp) ;; LSP is the dependency of language modules
(require 'elpa-eglot)

;; Load custom configuration which takes the highest precedence
(load custom-file :noerror)
(message "[pew] Normal init finished")

(provide 'init)
;;; init.el ends here
