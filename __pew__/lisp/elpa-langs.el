;;; elpa-langs.el --- random language supports -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loading for these languages

(use-package vimrc-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package json-mode :ensure t :defer t)
(use-package fish-mode :ensure t :defer t)
(use-package nix-mode :ensure t :defer t)
(use-package csharp-mode :ensure t :defer t)
(use-package powershell :ensure t :defer t)
(use-package python-mode :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)

;;; Common

(use-package cmake-mode
  :ensure t
  :defer t
  :custom
  cmake-tab-width 4)

(use-package lua-mode
  :ensure t
  :defer t
  :custom
  (lua-indent-level 2))

(use-package cc-mode
  :ensure nil
  :hook ((c-mode . pew::cc-mode::on-enter)
         (c++-mode . pew::cc-mode::on-enter))
  :preface
  ;; Setup functions
  (defun pew::cc-mode::on-enter ()
    "Common CC mode setup."
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'innamespace 0)
    ;; Indentation
    (setq-local indent-tabs-mode nil
                c++-tab-always-indent t
                c-basic-offset 4
                c-indent-level 4
                tab-width 4
                tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
                c-syntactic-indentation t
                c-syntactic-indentation-in-macros t
                ;; Fill columns
                adaptive-fill-mode nil
                ;; Macro line continuation
                c-backslash-column 80
                c-backslash-max-column 160
                c-auto-align-backslashes t)))

(use-package plantuml-mode
  :ensure t
  :mode (("\\.puml\\'" . plantuml-mode)
         ("\\.plantuml\\'" . plantuml-mode))
  :custom
  (plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
  (plantuml-default-exec-mode 'jar)

  :init
  ;; `org-mode' support
  (with-eval-after-load 'org
    (setq org-plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
    (setq org-plantuml-exec-mode 'jar)
    (pew::org::add-src-lang-modes '(("plantuml" . plantuml)))
    (pew::org::add-babel-load-languages '((plantuml . t)))))

(use-package graphviz-dot-mode
  :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :custom
  (graphviz-dot-indent-width 4)

  :init
  ;; Graphviz `org-mode' support
  (with-eval-after-load 'org
    (pew::org::add-src-lang-modes '(("dot" . graphviz-dot)
                                    ("gv" . graphviz-dot)))
    (pew::org::add-babel-load-languages '((dot . t)))))

(use-package mermaid-mode
  :ensure t
  :mode ("\\.mmd\\'" . mermaid-mode)
  :custom
  (mermaid-mmdc-location (expand-file-name ".cache/mermaid/node_modules/.bin/mmdc" user-emacs-directory))
  :init
  (defun pew::mermaid-mode::install-cli ()
    "Install Mermaid CLI tool in user Emacs folder."
    (interactive)
    (let ((default-directory (substring mermaid-mmdc-location 0 (string-match-p "node_modules" mermaid-mmdc-location))))
      (message "Installing mermaid-cli...")
      (mkdir default-directory t)
      (call-process "npm" nil nil nil "install" "@mermaid-js/mermaid-cli")
      (message "Installing mermaid-cli... done"))))

;; Mermaid `org-mode' support
(use-package ob-mermaid
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'org
    (pew::org::add-src-lang-modes '(("mermaid" . mermaid)))
    (pew::org::add-babel-load-languages '((mermaid . t)))))

(provide 'elpa-langs)
;;; elpa-langs.el ends here
