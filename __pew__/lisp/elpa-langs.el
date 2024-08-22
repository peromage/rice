;;; elpa-langs.el --- random language supports -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loading for these languages
(pewcfg::defer-use-packages
  vimrc-mode
  yaml-mode
  json-mode
  fish-mode
  nix-mode
  csharp-mode
  powershell
  python-mode)

;;; CMake
(use-package cmake-mode
  :defer t
  :custom
  cmake-tab-width 4)

;;; Lua
(use-package lua-mode
  :defer t
  :custom
  (lua-indent-level 2))

;;; C/C++
(use-package cc-mode
  :defer t
  :hook ((c-mode . pew::cc-mode::on-enter)
         (c++-mode . pew::cc-mode::on-enter))
  :config
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

;;; Markdown
(use-package markdown-mode
  :defer t
  :hook (markdown-mode . pew::markdown-mode::on-enter)
  :config
  (defun pew::markdown-mode::on-enter ()
    "`markdown-mode' initialization."
    (pewlib::as-text-mode)))

;;; Plantuml
(use-package plantuml-mode
  :defer t
  :mode (("\\.puml\\'" . plantuml-mode)
         ("\\.plantuml\\'" . plantuml-mode))
  :custom
  (plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
  (plantuml-default-exec-mode 'jar))

;; Plantuml `org-mode' support
(pewcfg::use-package-fragment org
  :custom
  (org-plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
  (org-plantuml-exec-mode 'jar)
  :config
  (pew::org::add-src-lang-modes '(("plantuml" . plantuml)))
  (pew::org::add-babel-load-languages '((plantuml . t))))

;;; Graphviz
(use-package graphviz-dot-mode
  :defer t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :custom
  (graphviz-dot-indent-width 4))

;; Graphviz `org-mode' support
(pewcfg::use-package-fragment org
  :config
  (pew::org::add-src-lang-modes '(("dot" . graphviz-dot)
                                  ("gv" . graphviz-dot)))
  (pew::org::add-babel-load-languages '((dot . t))))

;;; Mermaid
(use-package mermaid-mode
  :defer t
  :mode (("\\.mmd\\'" . mermaid-mode))
  :custom
  (mermaid-mmdc-location (expand-file-name ".cache/mermaid/node_modules/.bin/mmdc" user-emacs-directory))
  :config
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
  :after (:all org mermaid-mode))

(pewcfg::use-package-fragment org
  :config
  (pew::org::add-src-lang-modes '(("mermaid" . mermaid)))
  (pew::org::add-babel-load-languages '((mermaid . t))))

(provide 'elpa-langs)
;;; elpa-langs.el ends here
