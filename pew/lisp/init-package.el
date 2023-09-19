;;; init-package.el --- package management -*- lexical-binding: t; -*-
;;; Commentary:

;; This ELPA initialization configuration should be loaded as early as possible.

;;; Code:

;;; Builtin package manager
(require 'package)

;; Config
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

;;; `use-package'
(eval-and-compile
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package))

;; Default `use-package' behaviors
(setq use-package-always-ensure t
      use-package-always-defer nil
      use-package-always-demand nil
      use-package-always-pin nil
      use-package-compute-statistics nil
      use-package-verbose nil)

;; A convenient function to enable custom values from the synthetic theme
(defun use-package--enable-custom-theme ()
  "A quick way to enable all the settings from the `use-package' theme.
After enabling, remove the synthetic theme from the enabled themes, so iterating
over them to disable-all-themes won't disable it."
  (interactive)
  (let ((theme 'use-package))
    (enable-theme theme)
    (setq custom-enabled-themes (remq theme custom-enabled-themes))))

(provide 'init-package)
;;; init-package.el ends here
