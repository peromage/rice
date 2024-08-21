;;; elpa-lang-graphviz.el --- graphviz dot mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: graphviz-dot-mode
(pewcfg::use-package graphviz-dot-mode
  :defer t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :custom
  (graphviz-dot-indent-width 4))

;;; `org-mode' support
(pewcfg::use-package-fragment org
  :config
  (pew::org::add-src-lang-modes '(("dot" . graphviz-dot)
                                  ("gv" . graphviz-dot)))
  (pew::org::add-babel-load-languages '((dot . t))))

(provide 'elpa-lang-graphviz)
;;; elpa-lang-graphviz.el ends here
