;;; elpa-treesit.el --- Treesitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Builtin tree-sitter
(use-package treesit :ensure nil)

;; Remember to run `treesit-langs-install-grammars' for the first time
(use-package treesit-langs
  :ensure nil ;; Install from repo instead
  :commands (treesit-langs-major-mode-setup treesit-langs-install-grammars)
  :init (pewcfg::vc-install "emacs-tree-sitter/treesit-langs" "main"))

(provide 'elpa-treesit)
;;; elpa-treesit.el ends here
