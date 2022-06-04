;;; elpa-lang.el --- Language support -*- lexical-binding: t -*-
;;; Commentary:
;; A simple configuration collection for major modes

;;; Code:
;; If the mode needs to be configured further, move it to an individual module.
(use-package vimrc-mode :defer t)
(use-package yaml-mode :defer t)
(use-package json-mode :defer t)

(provide 'elpa-lang)
;;; elpa-lang.el ends here
