;;; init.el --- Configuration entry -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files.  Templates was from purcell/emacs.d

;;; Code:

;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(let ((minimal-emacs-version "27"))
  (when (version< emacs-version minimal-emacs-version)
    (error "Emacs' version is too old.  Please use %s and above" minimal-emacs-version)))

(setq pew/home-dir (file-name-directory load-file-name)
      pew/temp-dir (expand-file-name "tempfiles" pew/home-dir)
      ;; Variables configured via the interactive 'customize' interfaces
      custom-file (expand-file-name "local.el" pew/home-dir))

;; Runtime path
(add-to-list 'load-path (expand-file-name "lisp" pew/home-dir))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Start a daemon if it is not running yet
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (message "[pew] Starting Emacs daemon")
              (server-start))))

;; Create the temporary directory
(unless (file-directory-p pew/temp-dir)
  (make-directory pew/temp-dir t))

;;------------------------------------------------------------------------------
;; Bootstrap -- To avoid  nested loading, all packages are managed here
;;------------------------------------------------------------------------------

;; Base setup
(require 'init-shared)
(require 'init-custom)
(require 'init-keymaps)
(require 'init-package)

;; Framework
(require 'pkg-ivy)

;; Vim keymaps
(require 'pkg-evil)

;; Navigation
(require 'pkg-which-key)
(require 'pkg-projectile)

;; Git
(require 'pkg-magit)
(require 'pkg-git-gutter)

;; Coding
(require 'pkg-flycheck)
(require 'pkg-company)
(require 'pkg-yasnippet)
(require 'pkg-major-modes)

;; LSP
(require 'pkg-lsp)
(require 'pkg-lsp-cc)
(require 'pkg-lsp-python)
(require 'pkg-lsp-cs)
(require 'pkg-lsp-powershell)

;; Other packages
(require 'pkg-vterm)
(require 'pkg-edwina)

;; Internal packages
(require 'pkg-dired)
(require 'pkg-ediff)
(require 'pkg-electric)
(require 'pkg-eshell)
;;(require 'pkg-ido)
(require 'pkg-org)

;; Appearance
(require 'pkg-themes)

;; Load this at the last to prevent local configurations from being overridden
(when (file-exists-p custom-file) (load custom-file))

(provide 'init)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; no-byte-compile: t
;;; End:
;;; init.el ends here
