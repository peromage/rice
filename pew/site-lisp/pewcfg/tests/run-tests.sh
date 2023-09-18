#!/usr/bin/env -S emacs --batch --script
;;; run-tests.sh --- unit tests entry -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Process arguments
(if (< (length argv) 1) (error "Not enough arguments"))
(setq repo-root-path (nth 0 argv))

;;; Load paths
(setq load-path (nconc (mapcar (lambda (p) (expand-file-name p repo-root-path))
                               '("pew/lisp"
                                 "pew/site-lisp"
                                 "pew/site-lisp/pewcfg"
                                 "pew/site-lisp/pewcfg/tests"))
                       load-path))

;;; Load required modules
(require 'common-test-defs)
(require 'pewcfg)
(require 'test-pewcfg-core)
(require 'test-pewcfg-use-package)

(kill-emacs (execute-test-suites 'execute-suite-test-pewcfg-core
                                 'execute-suite-test-pewcfg-use-package))
