;;; init.el --- Configuration entry -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files.  Templates was from purcell/emacs.d

;;; Code:

;;------------------------------------------------------------------------------
;; Minimal version required
;;------------------------------------------------------------------------------

(let ((minimal-emacs-version "27"))
  (when (version< emacs-version minimal-emacs-version)
    (error "Emacs' version is too old.  Please use %s and above" minimal-emacs-version)))

;;------------------------------------------------------------------------------
;; Bootstrap
;;------------------------------------------------------------------------------

;; Core
(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory load-file-name)))
(require 'init-base)
(require 'init-lib)
(require 'init-keymap)
(require 'init-config)

;; Packages
(require 'pack-theme)
(require 'pack-vim)
(require 'pack-git)
(require 'pack-navigation)
(require 'pack-coding)
(require 'pack-org)
(require 'pack-terminal)

;; Variables configured via the interactive 'customize' interfaces
;; Load this at the last to prevent local configurations from being overridden
(setq custom-file (expand-file-name "local.el" pew/home-dir))
(pew/load-if-exists custom-file)

(provide 'init)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; no-byte-compile: t
;;; End:
;;; init.el ends here
