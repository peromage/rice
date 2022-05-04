;;; elpa-yasnippet.el --- Snippet management -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs missing snippet manager.
;; Default snippet directory is located at "snippets" in this PEW configuration.

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" pew/home-dir)))
  :config
  (yas-global-mode 1))

(provide 'elpa-yasnippet)
;;; elpa-yasnippet.el ends here
