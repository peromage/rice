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
