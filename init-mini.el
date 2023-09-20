;;; init-mini.el --- pew mini bootstraps -*- lexical-binding: t; coding: utf-8; no-byte-compile: t; -*-
;;; Commentary:
;; To use this setup, run
;;   emacs -q -l /path/to/init-mini.el
;;; Code:

;;; Configuration variables.
(defvar pew::minimal-emacs-version "28.1"
  "The minimal Emacs version that PEW supports.")

(defvar pew::home-dir (file-name-directory load-file-name)
  "The PEW configuration's home directory.
Not necessarily to be `user-emacs-directory' since this configuration can be
loaded from other places.")

;;; Emacs version check
(if (version< emacs-version pew::minimal-emacs-version)
    (error "[pew] Emacs version %s+ is required" pew::minimal-emacs-version))

;;; Emacs variables
;; The runtime path should be relative to `pew::home-dir' instead of `user-emacs-directory'
(setq load-path (nconc (mapcar (lambda (p) (expand-file-name p pew::home-dir))
                               '("pew/lisp"
                                 "pew/site-lisp"
                                 "pew/site-lisp/pewcfg"))
                       load-path))

(require 'pewcfg)
(require 'init-common)
(require 'init-defaults)

(pewcfg
  :customize
  (icomplete-vertical-mode t))

(message "[pew] Minimal init finished")

(provide 'init-mini)
;;; init-mini.el ends here
