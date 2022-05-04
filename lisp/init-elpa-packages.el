;;; init-elpa-packages.el --- Initialize ELPA packages -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain only ELPA packages.
;; If package installation is not handled by `use-package' then the installation should happen before requiring package configuration.

;;; Code:

;;;; Package installation

;; TODO: Package list
;; TODO: Install Packages

;;;; Load package configurations

;; Vim keymaps
(require 'elpa-evil)

;; Navigation
(require 'elpa-ivy)
;(require 'elpa-helm)
(require 'elpa-which-key)
(require 'elpa-projectile)
;(require 'elpa-edwina)
;(require 'elpa-eyebrowse)
;(require 'elpa-treemacs)

;; Git
(require 'elpa-magit)
(require 'elpa-git-gutter)

;; Coding
(require 'elpa-flycheck)
(require 'elpa-company)
(require 'elpa-yasnippet)
(require 'elpa-lsp)

;; Other packages
(require 'elpa-vterm)

;; Major modes
(require 'elpa-mode-cc)
(require 'elpa-mode-cs)
(require 'elpa-mode-python)
(require 'elpa-mode-powershell)
(require 'elpa-mode-vimrc)
(require 'elpa-mode-markdown)

;; Appearance
(require 'elpa-themes)
(require 'elpa-theme-doom-modeline)

(provide 'init-elpa-packages)
;;; init-elpa-packages.el ends here
