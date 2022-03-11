;;; pkg-lsp.el --- Language Server Protocol supports -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;------------------------------------------------------------------------------
;; LSP core package
;;------------------------------------------------------------------------------

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
         ("C-c j" . lsp-find-definition)
         ("C-c J" . lsp-find-declaration))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-snippet nil)
  (lsp-auto-configure t)
  (lsp-enable-symbol-highlighting t)
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-idle-delay 0.5)
  ;(lsp-keymap-prefix "C-c l")
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-render-all nil)
  ;(lsp-signature-auto-activate nil)
  (lsp-signature-doc-lines 1)
  (lsp-signature-render-documentation nil)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-code-actions-enable t)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-headerline-breadcrumb-enable t)
  :config
  (lsp-enable-which-key-integration 1))

;;------------------------------------------------------------------------------
;; LSP experience improvement
;;------------------------------------------------------------------------------

(defun pew/lsp-ui/doc-glance ()
  "Quick peek documentation for the current symbol."
  (interactive)
  (if (lsp-ui-doc--frame-visible-p)
      (lsp-ui-doc-focus-frame)
    (lsp-ui-doc-glance)))

(defun pew/lsp-ui/doc-toggle ()
  "Toggle doc frame."
  (interactive)
  (lsp-ui-doc-mode 'toggle))

(defun pew/lsp-ui/setup ()
  "Setup function for lsp-ui."
  (lsp-ui-mode 1)
  (lsp-ui-doc-frame-mode -1))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ("C-c u" . lsp-ui-imenu)
         ("C-c k" . pew/lsp-ui/doc-glance)
         ("C-c K" . pew/lsp-ui/doc-toggle))
  :hook (lsp-mode . pew/lsp-ui/setup)
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode 'point)
  (lsp-ui-sideline-delay 1)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-delay 0)
  (lsp-ui-imenu-window-width 0)
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-imenu-refresh-delay 1.0))

(if (featurep 'ivy)
    (use-package lsp-ivy
      :ensure t
      :commands lsp-ivy-workspace-symbol))

(provide 'pkg-lsp)
;;; pkg-lsp.el ends here
