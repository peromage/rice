;;; init-package.el --- package management -*- lexical-binding: t; -*-
;;; Commentary:
;; This ELPA initialization configuration should be loaded as early as possible.
;;; Code:

;;; Emacs builtin package manager
(require 'package)

(setq
 ;; Package contents
 package-enable-at-startup nil
 ;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
 package-user-dir (locate-user-emacs-file (format "elpa-%s.%s" emacs-major-version emacs-minor-version))
 ;; Package repositories
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa" . 100)
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
(setq use-package-always-ensure t
      use-package-always-defer nil
      use-package-always-demand nil
      use-package-always-pin nil
      use-package-compute-statistics nil
      use-package-verbose nil)

(provide 'init-package)
;;; init-package.el ends here
