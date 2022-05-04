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
      ;; Variables configured via the interactive 'customize' interfaces
      ;; Local changes that are not tracked by VCS can go in here as well
      ;; Although some custom variables will be loaded twice the startup speed
      ;; is not dragged down that much. I can live with that.
      custom-file (expand-file-name "local.el" user-emacs-directory))

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

;;------------------------------------------------------------------------------
;; Bootstrap -- To avoid  nested loading, all packages are managed here
;;------------------------------------------------------------------------------

;; Base setup
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
