;;; pkg-yasnippet.el --- Snippet management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :defer t
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" pew/home-dir)))
  (yas-global-mode 1))

(provide 'pkg-yasnippet)
;;; pkg-yasnippet.el ends here
