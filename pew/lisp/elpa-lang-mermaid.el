;;; elpa-lang-mermaid.el --- mermaid mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: mermaid-mode
(pewcfg::use-package mermaid-mode
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

;;; Package: ob-mermaid -- org support
(pewcfg::use-package ob-mermaid
  :after (:all org mermaid-mode)
  :config
  (pewcfg::use-package-depend org
    :config
    (pew::org::add-src-lang-modes '(("mermaid" . mermaid)))
    (pew::org::add-babel-load-languages '((mermaid . t)))))

(provide 'elpa-lang-mermaid)
;;; elpa-lang-mermaid.el ends here
