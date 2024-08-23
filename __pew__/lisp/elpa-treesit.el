;;; elpa-treesit.el --- Treesitter -*- lexical-binding: t; -*-
;;; Commentary:

;; To get started, the grammar libraries must installed for the first time.
;; Use 'treesit-langs-install-grammars' to install a pre-built pack of grammar
;; from `treesit-langs.el'.
;;
;; For anything that is missing from above, use `treesit-install-language-grammar'
;; to install additional grammar.  Note that this requires local compilation.
;;
;; To check if a grammar is supported, use (treesit-language-available-p 'name).

;;; Code:

;;; Builtin tree-sitter
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4) ;; Maximize font rendering

  (treesit-language-source-alist
   '((elisp "https://github.com/Wilfred/tree-sitter-elisp" "main")
     (kdl "https://github.com/tree-sitter-grammars/tree-sitter-kdl" "master")))

  ;; Map traditional major modes to tree-sitter major modes
  (major-mode-remap-alist
   '((c++-mode . c++-ts-mode)
     (c-mode . c-ts-mode)
     (csharp-mode . csharp-ts-mode)
     (java-mode . java-ts-mode)
     (js-mode . js-ts-mode)
     (sh-mode . bash-ts-mode)
     (cmake-mode . cmake-ts-mode)
     (yaml-mode . yaml-ts-mode)
     (toml-mode . toml-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode))))

;;; Grammar pack
(use-package treesit-langs
  :ensure nil ;; Install from repo instead
  :commands (treesit-langs-major-mode-setup treesit-langs-install-grammars)
  :init (pewcfg::vc-install "emacs-tree-sitter/treesit-langs" "main"))

;;; Auxiliary packages
(use-package treesit-fold
  :ensure nil
  :init (pewcfg::vc-install "emacs-tree-sitter/treesit-fold" "master")
  :config
  ;; (global-treesit-fold-indicators-mode) ;; Not working in terminal
  (global-treesit-fold-mode 1))

;;; Language tree-sitter major modes

(use-package kdl-ts-mode
  :ensure nil
  :mode (("\\.kdl\\'" . kdl-ts-mode))
  :init (pewcfg::vc-install "dataphract/kdl-ts-mode" "main"))

(provide 'elpa-treesit)
;;; elpa-treesit.el ends here
