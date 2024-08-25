;;; init-boot.el --- early initialization -*- lexical-binding: t; -*-
;;; Commentary:

;; This file should be required at the beginning of the entire initialization.

;;; Code:

;; Performance optimization
(setq gc-cons-threshold 100000000      ;; 128mb
      read-process-output-max 1000000) ;; 1mb

;; Daemon
(defun pew::start--daemon ()
  "Start Emacs daemon if not running."
  (require 'server)
  (if (server-running-p)
      (message "[pew] Started in foreground mode")
    (message "[pew] Started in daemon mode")
    (server-start)))
(add-hook 'after-init-hook #'pew::start--daemon)

(provide 'init-boot)
;;; init-boot.el ends here
