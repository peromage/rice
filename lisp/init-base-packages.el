;;; init-base-packages.el --- Initialize builtin packages -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain only builtin package configurations.
;; If the package requires any additional packages from ELPA it should be moved to init-elpa-packages.

;;; Code:

;; Internal packages
(require 'base-dired)
(require 'base-ediff)
(require 'base-electric)
(require 'base-eshell)
(require 'base-ibuffer)
;(require 'base-ido)
(require 'base-org)
;(require 'base-winner)

(provide 'init-base-packages)
;;; init-base-packages.el ends here
