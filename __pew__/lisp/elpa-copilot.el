;;; elpa-copilot.el --- GitHub Copilot -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :ensure nil
  :commands (copilot-mode global-copilot-mode)
  :init (pewcfg :vcpkg ("copilot-emacs/copilot.el" "main"))
  :bind ( :map copilot-completion-map
          ;; Avoid conflict with regular completion
          ("M-n" . copilot-next-completion)
          ("M-p" . copilot-previous-completion)
          ("M-k" . copilot-clear-overlay)
          ("M-j" . copilot-accept-completion)
          ("M-f" . copilot-accept-completion-by-word)
          ("M-e" . copilot-accept-completion-by-line) ))

(provide 'elpa-copilot)
;;; elpa-copilot.el ends here
