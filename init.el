;;; init.el --- PEW entry -*- lexical-binding: t; coding: utf-8; no-byte-compile: t; -*-

;;; Commentary:
;; This file bootstraps the PEW configuration which is divided into a number of
;; module files.  Inspired by purcell/emacs.d

;;; Code:
;;; Minimal version
(let ((minimal-emacs-version "28.1"))
  (if (version< emacs-version minimal-emacs-version)
      (error "[pew] Emacs version %s+ is required" minimal-emacs-version)))

;;; Config variables
(defvar pew/mini-init nil
  "When non-nil load PEW with the minimal configuration.")

(defvar pew/home-dir (file-name-directory load-file-name)
  "The PEW configuration's home directory.
Not necessarily to be `user-emacs-directory' since this configuration can be
loaded from other places.")

;;; Runtime path
;; The runtime path should be relative to this file instead of `user-emacs-directory'
(add-to-list 'load-path (expand-file-name "lisp" pew/home-dir))
(add-to-list 'load-path (expand-file-name "site-lisp" pew/home-dir))

;;; Module loading
(cond
;;;; Minimal setup
 (pew/mini-init
  (message "[pew] Loading minimal init")
  (require 'init-common)
  (require 'init-pewconfig)
  (require 'init-defaults))

;;;; Regular setup
 (t
  ;; Configurations from the interactive `customize' interfaces.
  ;; Any disposable code can be put in this file.
  (setq custom-file (locate-user-emacs-file "local.el"))
  ;; Load init files
  ;; NOTE: The load sequence must be in this order
  (require 'init-common)
  (require 'init-pewconfig)
  (require 'init-boot)
  (require 'init-defaults)
  (require 'init-package)
  ;; Load ELPA packages (managed by `use-package')
  (require 'elpa-evil)
  (require 'elpa-vertico)
  (require 'elpa-completion)
  (require 'elpa-git)
  (require 'elpa-org)
  (require 'elpa-org-utils)
  (require 'elpa-utils)
  (require 'elpa-ui)
  (require 'elpa-extra)
  ;; Language packages may depend on the packages required previously
  (require 'elpa-lsp)
  (require 'elpa-lang)
  (require 'elpa-lang-c)
  (require 'elpa-lang-csharp)
  (require 'elpa-lang-powershell)
  (require 'elpa-lang-python)
  (require 'elpa-lang-java)
  (require 'elpa-lang-lua)
  (require 'elpa-lang-markdown)
  (require 'elpa-lang-plantuml)
  ;; Load custom configuration which takes the highest precedence
  (load custom-file :noerror)))

(provide 'init)
;;; init.el ends here
