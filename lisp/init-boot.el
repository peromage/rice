;;; init-boot.el --- Boot environment -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures the environment while booting up.

;;; Code:

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(provide 'init-boot)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
