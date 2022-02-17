;;; init.el --- Configuration entry -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files.  Templates was from purcell/emacs.d

;;; Code:

;;------------------------------------------------------------------------------
;; Prerequisites
;;------------------------------------------------------------------------------

(let ((minimal-emacs-version "27"))
  (when (version< emacs-version minimal-emacs-version)
    (error "Emacs' version is too old.  Please use %s and above" minimal-emacs-version)))

(setq pew/home-dir (file-name-directory load-file-name)
      ;; Variables configured via the interactive 'customize' interfaces
      custom-file (expand-file-name "local.el" pew/home-dir))

;; Runtime path
(add-to-list 'load-path (expand-file-name "lisp" pew/home-dir))

;;------------------------------------------------------------------------------
;; Bootstrap -- To avoid  nested loading, all packages are managed here
;;------------------------------------------------------------------------------

;; Base setup
(require 'init-shared)
(require 'init-base)
(require 'init-custom)
(require 'init-keymaps)

;; Framework
(require 'pkg-ivy)

;; Vim keymaps
(require 'pkg-evil)

;; Navigation
(require 'pkg-which-key)
(require 'pkg-projectile)
(require 'pkg-dired)

;; Git
(require 'pkg-magit)
(require 'pkg-ediff)
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
(require 'pkg-org)
(require 'pkg-vterm)

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
