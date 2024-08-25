;;; elpa-minibuffer-vertico.el --- vertico and complementary -*- lexical-binding: t; -*-
;;; Commentary:

;; Recommended package bundles are from: https://github.com/minad/vertico#complementary-packages

;;; Code:

(use-package vertico
  :demand t
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word)
          ("M-1" . vertico-multiform-vertical)
          ("M-2" . vertico-multiform-grid)
          ("M-3" . vertico-multiform-flat)
          ("M-4" . vertico-multiform-reverse)
          ("M-5" . vertico-multiform-unobtrusive) )

  :custom
  ;; Default views for different commands/results
  ;; NOTE: `vertico-multiform-commands' takes precedence over `vertico-multiform-categories'
  (vertico-multiform-commands '((consult-imenu buffer indexed)
                                (consult-outline buffer)))
  (vertico-multiform-categories '((file (vertico-sort-function . pew::vertico::sort-directories-first))))

  :preface
  ;; File sorting
  (defun pew::vertico::sort-directories-first (files)
    "Sort directories before files."
    (let ((files (vertico-sort-history-length-alpha files)))
      (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
             (seq-remove (lambda (x) (string-suffix-p "/" x)) files))))

  :config
  (vertico-mode 1)
  (vertico-multiform-mode 1))

(use-package consult
  :demand t
  :bind ( ("C-s" . consult-line)
          ("C-x b" . consult-buffer)
          ("C-x B" . consult-buffer-other-window)
          ("C-x F" . consult-find)
          ("C-x g" . consult-ripgrep)
          ("C-x G" . consult-git-grep)
          ("C-x j" . consult-outline)
          ("C-x l" . consult-imenu)
          ("C-x L" . consult-flymake)
          ([remap imenu] . consult-imenu)
          ([remap goto-line] . consult-goto-line)
          ([remap bookmark-jump] . consult-bookmark)
          ([remap recentf-open-files] . consult-recent-file)
          ([remap evil-show-marks] . consult-mark)
          :map minibuffer-local-map
          ("M-h" . consult-history) )

  :custom
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (completion-in-region-function #'pew::consult::completion-in-region)
  (consult-async-min-input 2)
  ;; Disable all auto previews
  (consult-preview-key '("<up>" "<down>"))
  (consult-narrow-key "<")
  ;; Don't display special buffers
  (consult-buffer-filter (pewlib::map-buffer-regex pewlib::hidden-buffer-keywords))
  ;; Find hidden directory
  (consult-find-args "find . ! -path '*/.git/*'")

  :config
  ;; Enable preview for certain completions
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)

   consult-line
   consult-line-multi
   consult-outline
   consult-flymake
   :preview-key 'any)

  ;; Customized search
  (defun pew::consult::rg (dir args)
    "Like `consult-ripgrep' but with additional arguments.
Search directory DIR will be selected by a prompt.
ARGS should be a string of arguments passed to ripgrep."
    (interactive "DSearch directory: \nsrg args (-t/--type, -g/--glob, -./--hidden, --no-ignore ...): ")
    (let ((consult-ripgrep-args
           (format
            ;; Default arguments from `consult-ripgrep-args'
            "rg \
--null --line-buffered --color=never --max-columns=1000 \
--path-separator / --smart-case --no-heading --line-number \
%s ."
            args)))
      (consult-ripgrep dir)))

  ;; Completion in region replacement
  (defun pew::consult::completion-in-region (&rest args)
    "Use consult for region completion."
    (apply (if vertico-mode #'consult-completion-in-region #'completion--in-region) args))

  ;; CRM indicator
  (define-advice completing-read-multiple (:filter-args (args) pew::consult::crm-indicator)
    "Add an indicator for multi-occur mode."
    (cons (format "[CRM '%s'] %s" crm-separator (car args)) (cdr args)))) ;; End consult

(use-package marginalia
  :demand t ;; :bind would cause lazy loading which is not we expect
  :bind ( :map vertico-map
          ("M-m" . marginalia-cycle) )

  :config
  (marginalia-mode 1))

(use-package orderless
  :demand t
  :custom
  ;; (completion-category-overrides nil) ;; To use orderless exclusively
  ;; (completion-category-defaults nil)  ;; Together with above
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  (orderless-affix-dispatch-alist '((?~ . orderless-regexp)
                                    (?! . orderless-without-literal)
                                    (?^ . orderless-initialism)
                                    (?= . orderless-literal)
                                    (?? . orderless-flex)))
  :config
  (pewcfg :customize
          (completion-styles (nconc '(orderless) completion-styles))))

(use-package embark
  :hook (embark-collect-mode . pew::embark::on-enter-collect-mode)
  :bind ( ([remap describe-bindings] . embark-bindings)
          :map pew::M-u-map
          ("e a" . embark-act)
          ("e d" . embark-dwim)
          ("e e" . embark-export)
          ("e c" . embark-collect) )

  :custom
  (prefix-help-command #'embark-prefix-help-command)

  :preface
  (defun pew::embark::on-enter-collect-mode ()
    "`embark-collect-mode' initialization."
    (pewlib::reuse-window-in-buffer)
    (setq-local show-trailing-whitespace nil)))

(use-package embark-consult
  :after (:all consult embark))

(use-package wgrep
  :after (:all consult embark))

(provide 'elpa-minibuffer-vertico)
;;; elpa-minibuffer-vertico.el ends here
