#!/usr/bin/env -S emacs --batch --script
;;; test-init.sh --- Test startup with packages loaded -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Process arguments
(if (< (length argv) 1) (error "Not enough arguments"))
(setq init-dir (nth 0 argv))

;;; Test starts
(require 'url-vars)
(let* ((debug-on-error t)
       (url-show-status nil)
       (user-emacs-directory init-dir)
       (user-init-file (expand-file-name "init.el" user-emacs-directory))
       (load-path (delq user-emacs-directory load-path)))
  (load-file user-init-file)
  (setq dired-mode-hook nil) ;; Avoid failures due to hooks not being loaded in script mode
  (run-hooks 'after-init-hook)
  (run-hooks 'emacs-startup-hook)
  (message "%s started in %s" (emacs-version) (emacs-init-time)))
