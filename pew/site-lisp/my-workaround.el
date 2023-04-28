;;; my-workaround.el --- Additional workaround -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains temporary fixes for some configurations which do not take
;; effect somehow.
;; This should be loaded after init.el.
;; Can be loaded by `(load (locate-user-emacs-file "workaround.el"))'

;;; Code:
;; LSP somehow loads the old version of clangd.
(setq lsp-clangd-version "15.0.3")
;; These Evil settings somehow doesn't take effective from use-package.
(setq evil-want-Y-yank-to-eol t)
(setq evil-disable-insert-state-bindings t)
(setq evil-search-module 'evil-search)

(message "[pew] Loaded my-workaround")

(provide 'my-workaround)
;;; my-workaround.el ends here
