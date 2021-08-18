;;; init.el --- Configuration entry -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files. Templates was from purcell/emacs.d.

;;; Code:

;; Minimal version required
(when (version< emacs-version "26.1")
  (error "Emacs' version is too old. Please use 26.1 and above."))

;; Paths
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Bootstrap
(require 'init-user)
(require 'init-boot)
(require 'init-basic)
(require 'init-tui)
(require 'init-utils)
(require 'init-elpa)

;; Variables configured via the interactive 'customize' interface
;; Load this at the last to prevent local configurations from being overridden
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; no-byte-compile: t
;;; End:
