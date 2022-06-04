;;; init-boot.el --- Early initialization -*- lexical-binding: t -*-
;;; Commentary:

;; This file should be required at the beginning of the entire initialization.

;;; Code:

;; Check minimal version
(let ((minimal-emacs-version_ "28.1"))
  (when (version< emacs-version minimal-emacs-version_)
    (error "PEW only supports Emacs version %s and above" minimal-emacs-version_)))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold_ (* 20 1024 1024))
      (init-gc-cons-threshold_ (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold_)
  (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold normal-gc-cons-threshold_))))

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
