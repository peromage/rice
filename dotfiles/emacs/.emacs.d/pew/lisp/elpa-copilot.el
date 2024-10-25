;;; elpa-copilot.el --- GitHub Copilot -*- lexical-binding: t; -*-
;;; Commentary:

;; Based on the README, Node.js v18+ must be installed as a prerequisite.
;; For the first time installation, remember to invoke `copilot-install-server'
;; to install corresponding NPM package.

;; Known issues:
;;
;; - Cursor sometimes jumps to the end of Copilot prompt instead of the
;; beginning.  As a result, the `copilot-completion-map' will not take effective.
;; Workaround: Type the first a few letters to move the cursor to the right position.

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
