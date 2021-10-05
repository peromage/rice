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

;;==============================================================================
;; Paths
;;==============================================================================

;; Cache directory for local files
(let ((local-cache-dir (expand-file-name "cache" user-emacs-directory)))
  (unless (file-directory-p local-cache-dir)
      (make-directory local-cache-dir t)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "local.el" user-emacs-directory))

;;==============================================================================
;; Bootstrap
;;==============================================================================

;; Core settings
(require 'init-lib)
(require 'init-base)
(require 'init-config)
(require 'init-elpa)

;; Other packages
(require 'pack-theme)
(require 'pack-navigation)
(require 'pack-git)
(require 'pack-vim)
(require 'pack-typing)
(require 'pack-org)
(require 'pack-lsp)
(require 'pack-terminal)

;;==============================================================================
;; Wrapups
;;==============================================================================

;; Variables configured via the interactive 'customize' interfaces
;; Load this at the last to prevent local configurations from being overridden
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; no-byte-compile: t
;;; End:
;;; init.el ends here
