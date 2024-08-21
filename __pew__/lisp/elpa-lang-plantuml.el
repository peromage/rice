;;; elpa-lang-plantuml.el --- plantuml mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: plantuml-mode
(pewcfg::use-package plantuml-mode
  :defer t
  :mode (("\\.puml\\'" . plantuml-mode)
         ("\\.plantuml\\'" . plantuml-mode))

  :custom
  (plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
  (plantuml-default-exec-mode 'jar))

;;; `org-mode' support
(pewcfg::use-package-fragment org
  :custom
  (org-plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
  (org-plantuml-exec-mode 'jar)

  :config
  (pew::org::add-src-lang-modes '(("plantuml" . plantuml)))
  (pew::org::add-babel-load-languages '((plantuml . t))))

(provide 'elpa-lang-plantuml)
;;; elpa-lang-plantuml.el ends here
