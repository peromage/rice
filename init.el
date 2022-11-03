;;; init.el --- PEW entry -*- lexical-binding: t -*-
;;; Commentary:
;; This file bootstraps the PEW configuration which is divided into a number of
;; module files.  Inspired by purcell/emacs.d

;;; Code:
;;; Prerequisite
;; The runtime path should be relative to this file instead of `user-emacs-directory'
(let ((l/home (file-name-directory load-file-name)))
  (add-to-list 'load-path (expand-file-name "lisp" l/home))
  (add-to-list 'load-path (expand-file-name "site-lisp" l/home)))

;;; Config variables
(defvar pew/mini-init nil
  "When non-nil load PEW with the minimal configuration.")

;;; Module loading
(cond
 ;; Minimal setup
 (pew/mini-init
  (message "[pew] Loading minimal init")
  (require 'init-common)
  (require 'init-defaults))

 ;; Regular setup
 (t
  ;; Configurations from the interactive `customize' interfaces.
  ;; Any disposable code can be put in this file.
  (setq custom-file (locate-user-emacs-file "local.el"))
  ;; Load init files
  ;; NOTE: The load sequence must be in this order
  (require 'init-common)
  (require 'init-boot)
  (require 'init-defaults)
  (require 'init-package)
  ;; Load ELPA packages (managed by `use-package')
  (require 'elpa-evil)
  (require 'elpa-vertico)
  (require 'elpa-coding)
  (require 'elpa-writing)
  (require 'elpa-utils)
  (require 'elpa-git)
  (require 'elpa-ui)
  (require 'elpa-lsp)
  (require 'elpa-lang)
  (require 'elpa-extra)
  ;; Load custom configuration which takes the highest precedence
  (load custom-file :noerror)))

(provide 'init)
;;; Local Variables:
;;; mode: emacs-lisp
;;; coding: utf-8
;;; no-byte-compile: t
;;; End:
;;; init.el ends here
