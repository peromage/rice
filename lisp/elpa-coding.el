;;; elpa-coding.el --- Packages for coding tasks -*- lexical-binding: t -*-
;;; Commentary:

;; This module configures for common coding needs including syntax checker, completion and language support.

;;; Code:

;;;; Completion
;;;;; Company

;; This configures `company-mode' and enhances its experience.
(use-package company
  :demand t
  :diminish company-mode
  :bind (:map company-mode-map
         ("C-c i" . company-complete)
         ([remap completion-at-point] . company-complete)
         ([remap indent-for-tab-command] . company-indent-or-complete-common)
         :map company-active-map
         ("TAB" . company-complete)
         ("RET" . company-complete-selection)
         ("ESC" . company-abort)
         ("C-SPC" . company-search-abort)
         ("C-d" . company-show-doc-buffer)
         ("C-f" . company-show-location))
  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10)
  (company-tooltip-idle-delay 0.5)
  (company-idle-delay 0.0)
  (company-show-numbers t)
  (company-show-quick-access t)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-auto-complete nil)
  :config
  (global-company-mode 1)
  (company-tng-mode -1))

;;;;; Company Enhancement

(defun pew/company-box/doc-toggle (&optional status)
  "Toggle company box doc display if STATUS is omitted.
Otherwise if STATUS is given, the status of doc display depends on the value
of STATUS.  Possible values are:
  'show: Display doc.
  'hide: Do not display doc.
  Other values: Toggle doc display status."
  (interactive)
  (cond ((eq 'show status)
         (setq company-box-doc-enable t))
        ((eq 'hide status)
         (setq company-box-doc-enable nil))
        (t (setq company-box-doc-enable (not company-box-doc-enable)))))

(defun pew/company-box/on-start ()
  "Company-box on-start actions."
  (pew/company-box/doc-toggle 'hide))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :bind (:map company-active-map
         ("C-k" . company-box-doc-manually))
  :custom
  (company-box-doc-enable nil)
  (company-box-doc-delay 0.5)
  (company-box-enable-icon t)
  (company-box-color-icon t)
  (company-box-show-single-candidate 'always)
  (company-box-scrollbar t))

;;;; Syntax and spell checker

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode 1))

;;;; Snippets

;; Default snippet directory is located at "snippets" in this PEW configuration.
(use-package yasnippet
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" pew/home-dir)))
  :config
  (yas-global-mode 1))

;;;; Language support
;;;;; LSP

;; For specific language LSP supports, they should go into the major mode modules.
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
         ("C-c j" . lsp-find-definition)
         ("C-c J" . lsp-find-declaration))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-snippet t)
  (lsp-auto-configure t)
  (lsp-enable-symbol-highlighting t)
  (lsp-lens-enable t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-idle-delay 0.0)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-render-all nil)
  (lsp-signature-auto-activate t)
  (lsp-signature-doc-lines 1)
  (lsp-signature-render-documentation t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-code-actions-enable t)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-enable-on-type-formatting nil)
  :config
  (lsp-enable-which-key-integration 1))

;;;;; LSP experience improvement

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
  (lsp-ui-doc-delay 0.0)
  (lsp-ui-imenu-window-width 0)
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-imenu-refresh-delay 0.5))

(if (featurep 'ivy)
    (use-package lsp-ivy
      :commands lsp-ivy-workspace-symbol))

;;;; Format

(use-package electric
  :ensure nil
  :custom
   ;; No closing pair if the open pair precedes a non-whitespace character
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  :config
  (electric-pair-mode -1)
  (electric-indent-mode 1))

(provide 'elpa-coding)
;;; elpa-coding.el ends here
