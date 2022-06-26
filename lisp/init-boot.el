;;; init-boot.el --- Early initialization -*- lexical-binding: t -*-
;;; Commentary:
;; This file should be required at the beginning of the entire initialization.

;;; Code:
;; Check minimal version
(let ((LminimalEmacsVersion "28.1"))
  (if (version< emacs-version LminimalEmacsVersion)
      (error "PEW only supports Emacs version %s and above" LminimalEmacsVersion)))

;; Performance optimization
(setq gc-cons-threshold (* 128 1024 1024)    ;; 128mb
      read-process-output-max (* 1024 1024)) ;; 1mb

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
