;;; init.el --- PEW bootstraps -*- lexical-binding: t; coding: utf-8; no-byte-compile: t; -*-

;;; Commentary:
;; This file bootstraps the PEW configuration which is divided into a number of
;; module files.  Inspired by purcell/emacs.d

;;; Code:
;;; Configuration variables.
;; These variables can be overridden in the early file.
(defvar pew::minimal-emacs-version "28.1"
  "The minimal Emacs version that PEW supports.")

(defvar pew::mini-init nil
  "When non-nil load PEW with the minimal configuration.")

(defvar pew::home-dir (file-name-directory load-file-name)
  "The PEW configuration's home directory.
Not necessarily to be `user-emacs-directory' since this configuration can be
loaded from other places.")

(defvar pew::default-org-dir (expand-file-name "my-org-notes" pew::home-dir)
  "The default directory to place org note files.
Default to under `pew::home-dir'.")

(defvar pew::org-template-dir (expand-file-name "templates/org" pew::home-dir)
  "The directory where to put org template files.
Default to under `pew::home-dir'.")

(defvar pew::yasnippet-template-dir (expand-file-name "templates/yasnippet" pew::home-dir)
  "The directory where to put yasnippet template files.
Default to under `pew::home-dir'.")

(defvar pew::custom-file (expand-file-name "custom.el" pew::home-dir)
  "Custom file path.  This is loaded after this init.el.
Default to under `pew::home-dir'.")

(defvar pew::custom-mini-file (expand-file-name "custom-mini.el" pew::home-dir)
  "Custom file path.  This is loaded after this init.el.
Default to under `pew::home-dir'.")

(defvar pew::early-custom-file (expand-file-name "early-custom.el" pew::home-dir)
  "Early custom file path.  This is loaded before this init.el.
Default to under `pew::home-dir'.")

;;; Load early file
(load pew::early-custom-file :noerror)
;; Configurations from the interactive `customize' interfaces.
(setq custom-file (if pew::mini-init pew::custom-mini-file pew::custom-file))

;;; Emacs version check
(if (version< emacs-version pew::minimal-emacs-version)
    (error "[pew] Emacs version %s+ is required" pew::minimal-emacs-version))

;;; Runtime path
;; The runtime path should be relative to this file instead of `user-emacs-directory'
(add-to-list 'load-path (expand-file-name "lisp" pew::home-dir))
(add-to-list 'load-path (expand-file-name "site-lisp" pew::home-dir))

;;; Load required libraries
(require 'subr-x)

;;; Module loading
(cond
;;;; Minimal setup
 (pew::mini-init
  (require 'init-common)
  (require 'init-pewcfg)
  (require 'init-defaults)
  (message "[pew] Loaded minimal init"))

;;;; Regular setup
 (t
  ;; Load init files
  ;; NOTE: The load sequence must be in this order
  (require 'init-common)
  (require 'init-pewcfg)
  (require 'init-boot)
  (require 'init-defaults)
  (require 'init-package)
  ;; Load ELPA packages (managed by `use-package')
  (require 'elpa-ui)
  (require 'elpa-evil)
  (require 'elpa-completion)
  ;; (require 'elpa-completion-ivy)
  ;; (require 'elpa-completion-company)
  (require 'elpa-completion-vertico)
  (require 'elpa-completion-corfu)
  (require 'elpa-git)
  (require 'elpa-org)
  (require 'elpa-org-utils)
  (require 'elpa-utils)
  ;; Language supports
  (require 'elpa-lang)
  (require 'elpa-lang-lsp) ;; LSP is the dependency of language moduels
  (require 'elpa-lang-c)
  (require 'elpa-lang-csharp)
  (require 'elpa-lang-java)
  (require 'elpa-lang-lua)
  (require 'elpa-lang-markdown)
  (require 'elpa-lang-plantuml)
  (require 'elpa-lang-powershell)
  (require 'elpa-lang-python)
  (message "[pew] Loaded normal init")))

;;; Load custom configuration which takes the highest precedence
(load custom-file :noerror)

(provide 'init)
;;; init.el ends here
