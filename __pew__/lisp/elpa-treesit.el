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

;; Builtin tree-sitter
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
     (nix-mode . nix-ts-mode)
     (java-mode . java-ts-mode)
     (js-mode . js-ts-mode)
     (sh-mode . bash-ts-mode)
     (cmake-mode . cmake-ts-mode)
     (yaml-mode . yaml-ts-mode)
     (toml-mode . toml-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)))

  :config
  (pewcfg :switch
          ;; Debug indent rules when `treesit-explore-mode' is on
          (treesit--indent-verbose)))

;; Grammar pack
;; Note: Use `treesit-language-available-p' to check if grammar is installed
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
  (global-treesit-fold-mode 1)) ;; Automatically adds folding functions to `evil-fold-list'.


;;; Language tree-sitter major modes

(use-package c-ts-mode
  :ensure nil
  :hook ((c-ts-mode . pew::c-ts-mode::on-enter)
         (c++-ts-mode . pew::c-ts-mode::on-enter))
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style #'pew::c-ts-mode::indent-style)

  :preface
  (defun pew::c-ts-mode::indent-style ()
    "Customized indentation rules.
Source:
- https://www.reddit.com/r/emacs/comments/1bgdw0y/custom_namespace_indentation_in_ctsmode/
- https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
- See also: `treesit-simple-indent-presets'.

Note: Set `treesit--indent-verbose' to show which indent rules matched at the
current point when `treesit-explore-mode' is on."

    (append '(;; Do not indent preprocessor directives
              ((node-is "preproc") column-0 0)
              ;; Do not indent namespace children
              ((n-p-gp nil nil "namespace_definition") grand-parent 0))
            ;; Base rule
            (alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))

  (defun pew::c-ts-mode::on-enter ()
    "Common C/C++ TS mode preference."
    (setq-local indent-tabs-mode nil
                tab-width 4
                tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
                adaptive-fill-mode nil)))

(use-package nix-ts-mode :defer t)

(use-package kdl-ts-mode
  :ensure nil
  :mode (("\\.kdl\\'" . kdl-ts-mode))
  :init (pewcfg::vc-install "dataphract/kdl-ts-mode" "main"))

(provide 'elpa-treesit)
;;; elpa-treesit.el ends here
