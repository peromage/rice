#!/usr/bin/env -S emacs --batch --script
;;; test-init-mini.sh --- Test minimal startup -*- mode: emacs-lisp; lexical-binding: t; -*-
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
       (user-init-file (expand-file-name "init-mini.el" user-emacs-directory))
       (load-path (delq user-emacs-directory load-path)))
  (load-file user-init-file)
  (run-hooks 'after-init-hook)
  (run-hooks 'emacs-startup-hook)
  (message "%s started in %s" (emacs-version) (emacs-init-time)))
