;;; elpa-completion-corfu.el --- Completion by corfu -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimalistic completion framework

;;; Code:
(use-package corfu
  :demand t

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-separator ?\s) ;; M-SPC
  (corfu-preview-current 'insert)
  (corfu-preselect 'valid)
  (corfu-on-exact-match 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-delay '(0.5 . 0.2))


  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :after corfu

  :config
  (corfu-terminal-mode 1))

(provide 'elpa-completion-corfu)
;;; elpa-completion-corfu.el ends here
