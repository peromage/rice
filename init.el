;;; init.el --- pew bootstraps -*- lexical-binding: t; coding: utf-8; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;; Configuration variables.
(defvar pew::minimal-emacs-version "28.1"
  "The minimal Emacs version that PEW supports.")

(defvar pew::home-dir (file-name-directory load-file-name)
  "The PEW configuration's home directory.
Not necessarily to be `user-emacs-directory' since this configuration can be
loaded from other places.")

(defvar pew::default-org-dir (expand-file-name "my-org-notes" pew::home-dir)
  "The default directory to place org note files.
Default is under `pew::home-dir'.")

(defvar pew::org-template-dir (expand-file-name "pew/org-templates" pew::home-dir)
  "The directory where to put org template files.
Default is under `pew::home-dir'.")

(defvar pew::yasnippet-template-dir (expand-file-name "pew/yasnippet-templates" pew::home-dir)
  "The directory where to put yasnippet template files.
Default is under `pew::home-dir'.")

(defvar pew::custom-file (expand-file-name "custom.el" pew::home-dir)
  "Custom file path.  This is loaded after this init.el.
Default is under `pew::home-dir'.")

;;; Emacs version check
(if (version< emacs-version pew::minimal-emacs-version)
    (error "[pew] Emacs version %s+ is required" pew::minimal-emacs-version))

;;; Emacs variables
;; Configurations from the interactive `customize' interfaces.
(setq custom-file pew::custom-file)
;; The runtime path should be relative to `pew::home-dir' instead of `user-emacs-directory'
(setq load-path (nconc (mapcar (lambda (p) (expand-file-name p pew::home-dir))
                               '("pew/lisp"
                                 "pew/site-lisp"
                                 "pew/site-lisp/pewcfg"))
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
