#!/usr/bin/env -S emacs --batch -x
;;; test-startup.el --- Test startup with packages loaded -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:
;; Basic sanity test for configuration startup.

;;; Code:
;;; Process arguments
(if (< (length argv) 1) (error "Not enough arguments"))
(setq repo-root-path (nth 0 argv))

(require 'url-vars)
(let ((debug-on-error t)
      (url-show-status nil)
      (user-emacs-directory repo-root-path)
      (user-init-file (expand-file-name "init.el" user-emacs-directory))
      (load-path (delq user-emacs-directory load-path)))
  (load-file user-init-file)
  (run-hooks 'after-init-hook)
  (run-hooks 'emacs-startup-hook)
  (message "%s started in %s" (emacs-version) (emacs-init-time)))

(provide 'test-startup)
;;; test-startup.el ends here
