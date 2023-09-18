;;; init-package.el --- package management -*- lexical-binding: t; -*-
;;; Commentary:
;; This ELPA initialization configuration should be loaded as early as possible.
;;; Code:

;;; Emacs builtin package manager
;; Initialize package manager
(require 'package)

;; Package contents
(setq package-enable-at-startup nil)
;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir (locate-user-emacs-file (format "elpa-%s.%s" emacs-major-version emacs-minor-version)))
;; Package repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("melpa" . 100)
                                   ("gnu" . 99)
                                   ("nongnu" . 98)))

;; Fire up package.el
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; Use `use-package' to simplify package configuration
;; Install `use-package'
(eval-and-compile
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  (message "[pew] Loaded use-package"))

;; Default `use-package' behaviors
(setq use-package-always-ensure t)
(setq use-package-always-defer nil)
(setq use-package-always-demand nil)
(setq use-package-always-pin nil)
(setq use-package-compute-statistics nil)
(setq use-package-verbose nil)

(provide 'init-package)
;;; init-package.el ends here
