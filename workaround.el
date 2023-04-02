;;; workaround.el --- Additional workaround -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains temporary fixes for some configurations which do not take
;; effect somehow.
;; This should be loaded after init.el.
;; Can be loaded by `(load (locate-user-emacs-file "workaround.el"))'

;;; Code:
(pewcfg
  :eval
  (message "[pew] Loading workaround")
  :custom
  ;; LSP somehow loads the old version of clangd.
  (lsp-clangd-version "15.0.3")
  ;; These Evil settings somehow doesn't take effective from use-package.
  (evil-want-Y-yank-to-eol t)
  (evil-disable-insert-state-bindings t)
  (evil-search-module 'evil-search))

(provide 'workaround)
;;; workaround.el ends here
