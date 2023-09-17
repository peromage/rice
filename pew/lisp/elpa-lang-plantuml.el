;;; elpa-lang-plantuml.el --- PlantUML mode -*- lexical-binding: t; -*-

;;; Commentary:
;; PlantUML major mode configuration.

;;; Code:
(use-package plantuml-mode
  :mode (("\\.puml\\'" . plantuml-mode)
         ("\\.plantuml\\'" . plantuml-mode))

  :config
  (pewcfg
    :setq
    (plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
    (plantuml-default-exec-mode 'jar)
    :eval
    (pew::use-package-maybe org
      :config
      (pewcfg
        :setq
        (org-plantuml-jar-path plantuml-jar-path)
        (org-plantuml-exec-mode plantuml-default-exec-mode)
        :eval
        (pew::org::add-src-lang-modes '(("plantuml" . plantuml)))
        (pew::org::add-babel-load-languages '((plantuml . t)))))))

(provide 'elpa-lang-plantuml)
;;; elpa-lang-plantuml.el ends here
