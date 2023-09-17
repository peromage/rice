;;; elpa-lang.el --- Programming language support -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for programming languages.

;;; Code:
;;; Lazy loading
(pew::use-package-later
  vimrc-mode
  yaml-mode
  json-mode
  fish-mode)

;;; Random packages
(use-package cmake-mode
  :defer t

  :config
  (pewcfg
    :setq
    (cmake-tab-width 4)))

(provide 'elpa-lang)
;;; elpa-lang.el ends here
