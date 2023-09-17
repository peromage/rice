;;; elpa-lang.el --- random language supports -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loading for these languages
(pew::use-package-later
  vimrc-mode
  yaml-mode
  json-mode
  fish-mode)

;;; Random packages
(use-package cmake-mode :defer t
  :custom
  cmake-tab-width 4)

(provide 'elpa-lang)
;;; elpa-lang.el ends here
