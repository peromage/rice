;;; elpa-lang.el --- Programming language support -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for programming languages.

;;; Code:
;;; Submodules
;; LSP has to be loaded first as it is the dependency of language moduels
(require 'elpa-lang-lsp)

;; Languages
(require 'elpa-lang-c)
(require 'elpa-lang-csharp)
(require 'elpa-lang-java)
(require 'elpa-lang-lua)
(require 'elpa-lang-markdown)
(require 'elpa-lang-plantuml)
(require 'elpa-lang-powershell)
(require 'elpa-lang-python)

;;; Lazy load
(pew::use-package-later
  vimrc-mode
  yaml-mode
  json-mode)

;;; Random packages
(use-package cmake-mode :defer t
  :custom
  cmake-tab-width 4)

(provide 'elpa-lang)
;;; elpa-lang.el ends here
