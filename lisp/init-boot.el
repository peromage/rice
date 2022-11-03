;;; init-boot.el --- Early initialization -*- lexical-binding: t -*-
;;; Commentary:
;; This file should be required at the beginning of the entire initialization.

;;; Code:
;; Check minimal version
(let ((l/minimal-emacs-version "28.1"))
  (if (version< emacs-version l/minimal-emacs-version)
      (error "PEW only supports Emacs version %s and above" l/minimal-emacs-version)))

;; Performance optimization
(setq gc-cons-threshold 100000000)     ;; 128mb
(setq read-process-output-max 1000000) ;; 1mb

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
