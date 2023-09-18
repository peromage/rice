;;; elpa-lang.el --- random language supports -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loading for these languages
(pewcfg::use-package-defer
  vimrc-mode
  yaml-mode
  json-mode
  fish-mode)

;;; Random packages
(pewcfg::use-package cmake-mode
  :defer t
  :custom
  cmake-tab-width 4)

(provide 'elpa-lang)
;;; elpa-lang.el ends here
