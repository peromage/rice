;;; init-base.el --- Basic environment initialization -*- lexical-binding: t -*-
;;; Commentary:

;; This is the basic Emacs environment initialization e.g. GC, daemon and etc..

;;; Code:

;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Start a daemon if it is not running yet
(add-hook 'after-init-hook (lambda ()
                             (require 'server)
                             (unless (server-running-p)
                               (message "[pew] Starting Emacs daemon")
                               (server-start))))

;; Redirect temporary files
(let ((local-cache-dir (expand-file-name "tempfiles" pew/home-dir))
      (file-settings
       '((bookmark-default-file . "bookmarks")
         (recentf-save-file . "recentf")
         (nsm-settings-file . "network-security.data")
         (ido-save-directory-list-file . "ido.last")
         (lsp-session-file . ".lsp-session-v1")
         (projectile-cache-file . "projectile.cache")
         (projectile-known-projects-file . "projectile-bookmarks.eld"))))
  (unless (file-directory-p local-cache-dir)
    (make-directory local-cache-dir t))
  ;; Local files
  (dolist (settings file-settings)
    (eval `(setq ,(car settings) ,(expand-file-name (cdr settings) local-cache-dir)))))

;;------------------------------------------------------------------------------
;; Package support
;;------------------------------------------------------------------------------

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
      use-package-always-demand nil)
;; use-package's utilities
(use-package diminish)

(provide 'init-base)
;;; init-base.el ends here
