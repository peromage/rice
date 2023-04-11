;;; elpa-lang.el --- Programming language support -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for programming languages.

;;; Code:
;;; No brainer modes
(use-package vimrc-mode :defer t)

(use-package yaml-mode :defer t)

(use-package json-mode :defer t)

(use-package cmake-mode :defer t
  :custom
  cmake-tab-width 4)

(provide 'elpa-lang)
;;; elpa-lang.el ends here
