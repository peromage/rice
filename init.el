;;; init.el --- PEW entry -*- lexical-binding: t -*-

;;; Commentary:

;; This file bootstraps the PEW configuration which is divided into a number of
;; other files.  Templates was from purcell/emacs.d

;;; Code:
;;;; Preparation

;; The runtime path should be relative to this file instead of `user-emacs-directory'
(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory load-file-name)))

;; Configurations from the interactive `customize' interfaces.
;; Any disposable code can be put in this file.
(setq custom-file (expand-file-name "local.el" user-emacs-directory))

;;;; Load modules

;; The load sequence must be in order
(require 'init-common)
(require 'init-boot)
(require 'init-custom)
(require 'init-package)
;; Load ELPA packages (managed by `use-package')
(require 'elpa-evil)
(require 'elpa-navigation)
(require 'elpa-coding)
(require 'elpa-git)
(require 'elpa-shell)
(require 'elpa-ui)
(require 'elpa-lang)
(require 'elpa-lang-cc)
(require 'elpa-lang-csharp)
(require 'elpa-lang-python)
(require 'elpa-lang-powershell)
(require 'elpa-lang-org)
(require 'elpa-lang-markdown)

;; Load this at the last to prevent local configurations from being overridden
(load custom-file t)

(provide 'init)
;;; Local Variables:
;;; mode: ELisp
;;; coding: utf-8
;;; no-byte-compile: t
;;; End:
;;; init.el ends here
