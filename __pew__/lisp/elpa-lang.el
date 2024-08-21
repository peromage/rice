;;; elpa-lang.el --- random language supports -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loading for these languages
(pewcfg::use-package-defer
  vimrc-mode
  yaml-mode
  json-mode
  fish-mode
  nix-mode
  csharp-mode
  powershell
  python-mode)

;;; Language major modes with simple config
(pewcfg::use-package cmake-mode
  :defer t
  :custom
  cmake-tab-width 4)

(pewcfg::use-package lua-mode
  :defer t
  :custom
  (lua-indent-level 2))

(provide 'elpa-lang)
;;; elpa-lang.el ends here
