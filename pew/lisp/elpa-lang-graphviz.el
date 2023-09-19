;;; elpa-lang-graphviz.el --- graphviz dot mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: graphviz-dot-mode
(pewcfg::use-package graphviz-dot-mode
  ;; Extensions are: '.dot' and '.gv'
  :defer t
  :custom
  (graphviz-dot-indent-width 4)
  :config
  (pewcfg::use-package-depend org
    :config
    (pew::org::add-babel-load-languages '((dot . t)))))

(provide 'elpa-lang-graphviz)
;;; elpa-lang-graphviz.el ends here
