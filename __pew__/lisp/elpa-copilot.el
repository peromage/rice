;;; elpa-copilot.el --- GitHub Copilot -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :ensure nil
  :init (pewcfg :vcpkg ("copilot-emacs/copilot.el" "main"))
  :bind ( :map copilot-completion-map
          ("<tab>" . copilot-accept-completion)
          ("TAB"   . copilot-accept-completion) ))

(provide 'elpa-copilot)
;;; elpa-copilot.el ends here
