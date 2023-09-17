;;; elpa-lang-plantuml.el --- PlantUML mode -*- lexical-binding: t; -*-

;;; Commentary:
;; PlantUML major mode configuration.

;;; Code:
(use-package plantuml-mode
  :mode (("\\.puml\\'" . plantuml-mode)
         ("\\.plantuml\\'" . plantuml-mode))

  :custom
  (plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
  (plantuml-default-exec-mode 'jar)

  :config
  (pew::use-package-maybe org
    :custom
    (org-plantuml-jar-path plantuml-jar-path)
    (org-plantuml-exec-mode plantuml-default-exec-mode)
    :config
    (pew::org::add-src-lang-modes '(("plantuml" . plantuml)))
    (pew::org::add-babel-load-languages '((plantuml . t)))))

(provide 'elpa-lang-plantuml)
;;; elpa-lang-plantuml.el ends here
