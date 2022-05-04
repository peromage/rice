;;; init-boot.el --- Early initialization -*- lexical-binding: t -*-
;;; Commentary:

;; This file should be required at the beginning of the entire initialization.

;;; Code:

;; Check minimal version
(let ((minimal-emacs-version "27"))
  (when (version< emacs-version minimal-emacs-version)
    (error "Emacs' version is too old.  Please use %s and above" minimal-emacs-version)))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Start a daemon if it is not running yet
(defun pew/start--daemon()
  "Start Emacs daemon if not running."
  (require 'server)
  (unless (server-running-p)
    (message "[pew] Starting Emacs daemon")
    (server-start)))
(add-hook 'after-init-hook #'pew/start--daemon)

(provide 'init-boot)
;;; init-boot.el ends here
