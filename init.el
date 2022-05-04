;;; init.el --- Configuration entry -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files.  Templates was from purcell/emacs.d

;;; Code:

;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defvar pew/home-dir (file-name-directory load-file-name)
  "The PEW configuration's home directory.
Not necessarily to be `user-emacs-directory' since this configuration can be loaded from other places.")

;; Configurations from the interactive `customize' interfaces.
;; Any disposable code can be put in this file.
(setq custom-file (expand-file-name "local.el" user-emacs-directory))

;; Runtime path
(add-to-list 'load-path (expand-file-name "lisp" pew/home-dir))

;;------------------------------------------------------------------------------
;; Bootstrap -- To avoid  nested loading, all packages are managed here
;;------------------------------------------------------------------------------

;; Base setup
(require 'init-boot)
(require 'init-common)
(require 'init-custom)
(require 'init-keymaps)
(require 'init-package)
(require 'init-base-packages)
(require 'init-elpa-packages)

;; Load this at the last to prevent local configurations from being overridden
(load custom-file t)

(provide 'init)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; no-byte-compile: t
;;; End:
;;; init.el ends here
