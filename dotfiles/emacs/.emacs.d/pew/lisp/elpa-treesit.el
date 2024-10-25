;;; elpa-treesit.el --- Treesitter -*- lexical-binding: t; -*-
;;; Commentary:

;; To get started, the grammar libraries must installed for the first time.
;; Use 'treesit-langs-install-grammars' to install a pre-built pack of grammar
;; from `treesit-langs.el'.
;;
;; For anything that is missing from above, use `treesit-install-language-grammar'
;; to install additional grammar.  Note that this requires local compilation.
;;
;; To check if a grammar is supported, use `treesit-language-available-p'.
;;
;; To debug/customize indentation by leveraging syntax tree, set
;; `treesit--indent-verbose' to non-nil to show current matched indentation rule
;; at the echo area.  Additionally, turn on `treesit-explore-mode' to view the
;; tree.
;; NOTE: The rule list is read in sequence so more specific matchers should be
;; put at the front.  For example, n-p-gp should be earlier than parent-is.
;;
;; See `treesit-simple-indent-presets' for matcher and anchor definitions.
;; See also: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html

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
(use-package treesit-langs
  :ensure nil ;; Install from repo instead
  :commands (treesit-langs-major-mode-setup treesit-langs-install-grammars)
  :init (pewcfg :vcpkg ("emacs-tree-sitter/treesit-langs" "main")))


;;; Auxiliary packages

(use-package treesit-fold
  :ensure nil
  :init (pewcfg :vcpkg ("emacs-tree-sitter/treesit-fold" "master"))
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
See: https://www.reddit.com/r/emacs/comments/1bgdw0y/custom_namespace_indentation_in_ctsmode"
    (nconc '(;; Do not indent preprocessor directives
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

(use-package nix-ts-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-ts-mode)
  :custom
  (nix-ts-mode-indent-offset 2)
  :config
  (setf (alist-get 'nix nix-ts-mode-indent-rules)
        (nconc '(;; NOTE: query only takes 2 nodes (parent and child) and the node
                 ;; to be indented needs to be captured.
                 ((query ((inherited_attrs) @attr)) parent-bol nix-ts-mode-indent-offset)
                 ((query ((inherited_attrs (_) @attr))) grand-parent nix-ts-mode-indent-offset)
                 ((query ((if_expression ["then" "else"] @branch))) parent 0)
                 ((query ((let_expression "in" body: (_) @body))) prev-line nix-ts-mode-indent-offset)
                 ((query ((let_expression "in" @in))) parent-bol 0))
               (alist-get 'nix nix-ts-mode-indent-rules))))

(use-package kdl-ts-mode
  :ensure nil
  :mode ("\\.kdl\\'" . kdl-ts-mode)
  :init (pewcfg :vcpkg ("dataphract/kdl-ts-mode" "main"))
  :custom
  (kdl-ts-mode-indent-offset 2))

(provide 'elpa-treesit)
;;; elpa-treesit.el ends here
