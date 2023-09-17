;;; elpa-lang-mermaid.el --- Mermaid mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Mermaid major mode and org integration.

;;; Code:

(use-package mermaid-mode
  :mode (("\\.mmd\\'" . mermaid-mode))

  :config
  (pewcfg
    :setq
    (mermaid-mmdc-location (expand-file-name ".cache/mermaid/node_modules/.bin/mmdc" user-emacs-directory))
    :eval
    (defun pew::mermaid-mode::install-cli ()
      "Install Mermaid CLI tool in user Emacs folder."
      (interactive)
      (let ((default-directory (substring mermaid-mmdc-location 0 (string-match-p "node_modules" mermaid-mmdc-location))))
        (message "Installing mermaid-cli...")
        (mkdir default-directory t)
        (call-process "npm" nil nil nil "install" "@mermaid-js/mermaid-cli")
        (message "Installing mermaid-cli... done")))))

(use-package ob-mermaid
  :after org
  :config
  (pew::use-package-maybe org
    :config
    (pew::org::add-src-lang-modes '(("mermaid" . mermaid)))
    (pew::org::add-babel-load-languages '((mermaid . t)))))

(provide 'elpa-lang-mermaid)
;;; elpa-lang-mermaid.el ends here
