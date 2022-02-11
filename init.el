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
;; Bootstrap -- To avoid complexity, all packages are managed here
;;------------------------------------------------------------------------------

;; Base setup
(require 'init-shared)
(require 'init-base)
(require 'init-custom)
(require 'init-keymaps)
;; Basics
(require 'pack-theme)
(require 'pack-vim)
(require 'pack-git)
;; Navigation
(require 'pack-navigation)
(require 'pack-navigation-ivy)
;; Coding
(require 'pack-coding)
(require 'pack-coding-major)
(require 'pack-coding-lsp)
(require 'pack-coding-lsp-cc)
(require 'pack-coding-lsp-py)
(require 'pack-coding-lsp-cs)
(require 'pack-coding-lsp-pwsh)
;; Other packages
(require 'pack-org)
(require 'pack-terminal)

;; Load this at the last to prevent local configurations from being overridden
(when (file-exists-p custom-file) (load custom-file))

(provide 'init)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; no-byte-compile: t
;;; End:
;;; init.el ends here
