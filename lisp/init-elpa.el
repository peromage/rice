;;; init-elpa.el --- Configurations for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

;; Standard package repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; Fire up package.el
(setq package-enable-at-startup nil)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Use 'use-package' to simplify package configurations
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (message "loading use-package")
  (require 'use-package))
;; Make sure packages will be installed
(setq use-package-always-ensure t)

(provide 'init-elpa)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
