;;; init-package.el --- Package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; This ELPA initialization configuration should be loaded before any other package settings.
(require 'package)

;; Standard package repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) pew/home-dir)
      package-enable-at-startup nil)

;; Fire up package.el
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Use 'use-package' to simplify package configurations
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (message "[pew] Loading use-package")
  (require 'use-package))
;; Make sure future packages will be installed
(setq use-package-always-ensure t
      use-package-always-defer nil
      use-package-always-demand nil
      use-package-compute-statistics nil
      use-package-verbose nil)
;; use-package's utilities
(use-package diminish)

(provide 'init-package)
;;; init-package.el ends here
