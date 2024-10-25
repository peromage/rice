#!/usr/bin/env -S emacs --batch --script
;;; run-tests.sh --- unit tests entry -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

(require 'files)

;;; Process arguments
(if (< (length argv) 1)
    (error "No pewcfg root specified"))
(setq pewcfg-dir (file-truename (nth 0 argv)))

;;; Load paths
(add-to-list 'load-path pewcfg-dir)
(let ((default-directory pewcfg-dir))
  (normal-top-level-add-subdirs-to-load-path))

;;; Load required modules
(require 'pewcfg)
(require 'common-test-defs)
(require 'test-pewcfg-core)
(require 'test-pewcfg-use-package)

(kill-emacs (execute-test-suites 'execute-suite-test-pewcfg-core
                                 'execute-suite-test-pewcfg-use-package))
