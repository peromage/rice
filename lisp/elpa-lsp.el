;;; elpa-lsp.el --- Language Server Protocol -*- lexical-binding: t -*-

;;; Commentary:
;; LSP configuration

;;; Code:

;;; LSP mode
;; For specific language LSP supports, they should go into the major mode modules.
(use-package lsp-mode
  :init
  (defmacro pew/lsp/define-remote (server modes)
    "A shortcut to define LSP remote client.
SERVER is the base name of the server executable.
MODES is a list of major mode symbols."
    (let ((l/server-id (intern (format "%s-remote" server))))
      `(with-eval-after-load 'lsp-mode
         (lsp-register-client
          (make-lsp-client
           :new-connection (lsp-tramp-connection ,server)
           :major-modes ',modes
           :remote? t
           :server-id ',l/server-id)))))

  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
         ([remap xref-find-definitions] . lsp-find-definition)
         ([remap xref-find-references] . lsp-find-declaration))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-snippet nil) ;; Non-nil to enable parameter insertion
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-folding t)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-keep-workspace-alive nil)
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-idle-delay 0.5)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all nil)
  (lsp-signature-doc-lines 1)
  (lsp-signature-render-documentation nil)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-code-actions-enable t)
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-completion-show-label-description t)
  (lsp-completion-no-cache nil)
  (lsp-completion-provider :capf)
  (lsp-ui-peek-always-show :flycheck)
  (lsp-log-io nil))

;;; LSP experience improvement
(use-package lsp-ui
  :init
  (defun pew/lsp-ui/setup ()
    "Setup function for lsp-ui."
    (lsp-ui-mode 1)
    ;; Disabled since it occupies 'q'
    (lsp-ui-doc-frame-mode -1))

  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ("C-c w w" . lsp-ui-doc-glance)
         ("C-c w e" . lsp-ui-doc-show)
         ("C-c w j" . lsp-ui-doc-focus-frame))
  :hook (lsp-mode . pew/lsp-ui/setup)
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode 'point)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-imenu-window-width 0)
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-imenu-auto-refresh-delay 0.5))

(provide 'elpa-lsp)
;;; elpa-lsp.el ends here
