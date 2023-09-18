;;; elpa-lang-lsp.el --- language server protocol -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: lsp-mode
(pewcfg::use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  ;; Features
  (lsp-enable-snippet t) ;; Non-nil to enable parameter insertion
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil) ;; No auto insertion (headers)
  (lsp-enable-folding t)
  (lsp-enable-indentation nil) ;; Don't get indented by server
  (lsp-enable-links nil) ;; Remove underline
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  ;; Shutdown server automatically
  (lsp-keep-workspace-alive nil)
  ;; User interface
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-workspace-status-enable t)
  (lsp-idle-delay 0.5)
  ;; Documentation and signature
  ;; M-n/M-p to scroll
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all nil)
  (lsp-signature-doc-lines 1) ;; Show some brief
  (lsp-signature-render-documentation t)
  ;; Completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-completion-show-label-description t)
  (lsp-completion-no-cache nil)
  (lsp-completion-provider :capf)
  ;; Other
  (lsp-log-io nil)

  :init
  (defmacro pew::lsp::define-remote (server modes)
    "A shortcut to define LSP remote client.
SERVER is the base name of the server executable.
MODES is a list of major mode symbols."
    (let ((l:server-id (intern (format "%s-remote" server))))
      `(with-eval-after-load 'lsp-mode
         (lsp-register-client
          (make-lsp-client
           :new-connection (lsp-tramp-connection ,server)
           :major-modes ',modes
           :remote? t
           :server-id ',l:server-id)))))) ;; End lsp-mode

;;; Package: lsp-ui
(pewcfg::use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . pew::lsp-ui::oninit)
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ;; Same prefix with lsp-mode-map
         ("C-c l l" . lsp-ui-doc-glance)
         ("C-c l L" . lsp-ui-doc-show)
         ("C-c l j" . lsp-ui-doc-focus-frame))

  :custom
  ;; Sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-delay 0.5)
  ;; Peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-peek-always-show t)
  ;; Doc
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  ;; imenu
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-imenu-auto-refresh-delay 0.5)
  (lsp-ui-imenu-window-fix-width nil)
  (lsp-ui-imenu-window-width 0)
  (lsp-ui-imenu-buffer-position 'right)
  (lsp-ui-imenu-kind-position 'top)

  :config
  (defun pew::lsp-ui::oninit ()
    "`lsp-ui-mode' initialization."
    (lsp-ui-mode 1)
    ;; Disabled since it occupies 'q'
    (lsp-ui-doc-frame-mode -1))) ;; End lsp-ui

;;; Package: dap-mode
(pewcfg::use-package dap-mode
  :defer t
  :custom
  (dap-python-executable "python3"))

(provide 'elpa-lang-lsp)
;;; elpa-lang-lsp.el ends here
