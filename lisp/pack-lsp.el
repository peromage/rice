;;; pack-lsp.el --- Language Server Protocol supports -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :commands lsp
  :bind
  (:map lsp-mode-map
        ("C-c j" . lsp-find-definition)
        ("C-c k" . lsp-find-declaration))
  :init
  (setq lsp-auto-configure t
        lsp-enable-symbol-highlighting t
        lsp-lens-enable t
        lsp-headerline-breadcrumb-enable t
        lsp-idle-delay 0.5
        lsp-session-file (expand-file-name "cache/.lsp-session-v1" user-emacs-directory)
        lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration 1))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ("C-c u" . lsp-ui-imenu))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-sideline-delay 0.2
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-delay 0.2
        lsp-eldoc-enable-hover nil
        lsp-modeline-diagnostics-enable t
        lsp-modeline-code-actions-enable t
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-headerline-breadcrumb-enable t))

(use-package lsp-ivy
  :requires ivy
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)

(require 'pack-lsp-c)

(provide 'pack-lsp)
;;; pack-lsp.el ends here
