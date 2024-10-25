;;; init-mini.el --- pew mini bootstraps -*- lexical-binding: t; coding: utf-8; no-byte-compile: t; -*-
;;; Commentary:
;; To use this setup, run
;;   emacs -q -l /path/to/init-mini.el
;;; Code:

;;; Emacs version check
(let ((min-ver "29"))
  (if (version< emacs-version min-ver)
      (error "[pew] Emacs version %s+ is required" min-ver)))

;;; Emacs variables
(let ((topLevel (expand-file-name "pew" (file-name-directory load-file-name))))
  ;; This config
  (setq load-path (nconc (mapcar (lambda (p) (expand-file-name p topLevel))
                                 '("lisp" "site-lisp/pewcfg" "site-lisp/pewlib"))
                         load-path)))

(require 'pewcfg-core)
(require 'pewlib)
(require 'init-config)

(pewcfg
  :customize
  (icomplete-vertical-mode t))

(message "[pew] Minimal init finished")

(provide 'init-mini)
;;; init-mini.el ends here
