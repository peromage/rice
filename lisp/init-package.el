;;; init-package.el --- Package management -*- lexical-binding: t -*-
;;; Commentary:
;; This ELPA initialization configuration should be loaded before any other package configurations.

;;; Code:
;;; Emacs builtin package manager
;; Initialize package manager
(require 'package)

;; Package contents
(setq package-enable-at-startup nil
      ;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
      package-user-dir (locate-user-emacs-file (format "elpa-%s.%s" emacs-major-version emacs-minor-version))
      ;; Package repositories
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '(("melpa" . 100)
                                   ("gnu" . 99)))

;; Fire up package.el
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; Use `use-package' to simplify package configuration
;; Install `use-package'
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (message "[pew] Loading use-package")
  (require 'use-package)
  (defmacro use-package? (name &rest args)
    "Configure a package but defer loading and don't install automatically.
This is useful when a package also requires some configurations in other packages
without modifying other packages' `use-package' forms but can keep using the same
syntax from `use-package' (unlike `with-eval-after-load')."
    (declare (indent 1))
    `(use-package ,name :ensure nil :defer t ,@args)))

;; Default `use-package' behaviors
(setq use-package-always-ensure t
      use-package-always-defer nil
      use-package-always-demand nil
      use-package-always-pin nil
      use-package-compute-statistics nil
      use-package-verbose nil)

;; `use-package' complementary packages
(use-package diminish :ensure t)

(provide 'init-package)
;;; init-package.el ends here
