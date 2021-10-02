;;; init-base.el --- Basic environment initialization -*- lexical-binding: t -*-
;;; Commentary:

;; This is the basic Emacs environment initialization e.g. GC, daemon and etc..

;;; Code:

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Allow access from the emacsclient.
(defun pew/start--emacs-daemon ()
  "Start a daemon if it is not running yet."
  (require 'server)
  (unless (server-running-p)
    (message "[pew] Starting Emacs daemon...")
    (server-start)))

(add-hook 'after-init-hook #'pew/start--emacs-daemon)

(provide 'init-base)
;;; init-base.el ends here
