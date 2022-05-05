;;; init-package.el --- Package management -*- lexical-binding: t -*-
;;; Commentary:

;; This ELPA initialization configuration should be loaded before any other package configurations.

;;; Code:

;; Initialize package manager
(require 'package)

;; Package repositories
(dolist (archive '(("melpa" . "https://melpa.org/packages/")
                   ;("melpa-stable" . "https://stable.melpa.org/packages/")
                   ))
  (add-to-list 'package-archives archive t))

;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) user-emacs-directory)
      package-enable-at-startup nil)

;; Fire up package.el
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Use "use-package" to simplify package configurations
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  (package-install 'diminish))
(eval-when-compile
  (message "[pew] Loading use-package")
  (require 'use-package)
  (require 'diminish))

;; Default `use-package' behaviors
(setq use-package-always-ensure t
      use-package-always-defer nil
      use-package-always-demand nil
      use-package-always-pin nil
      use-package-compute-statistics nil
      use-package-verbose nil)

(provide 'init-package)
;;; init-package.el ends here
