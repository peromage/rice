;;; elpa-completion-corfu.el --- Completion by corfu -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimalistic completion framework

;;; Code:
(use-package corfu
  :demand t

  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ("<tab>" . corfu-next)
         ("S-TAB" . corfu-previous)
         ("<backtab>" . corfu-previous)
         ("C-s" . corfu-insert-separator)
         ("C-c" . corfu-insert)
         ("C-k" . corfu-quit)
         ("RET" . corfu-insert)
         ("C-j" . corfu-move-to-minibuffer))

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-separator ?\s) ;; M-SPC
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match 'insert)
  (corfu-quit-at-boundary 'separator) ;; Quit boundary unless separator is used
  (corfu-quit-no-match 'separator) ;; Same above
  (corfu-popupinfo-delay '(0.5 . 0.2))


  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)) ;; (use-package corfu)

(use-package corfu-terminal
  :after corfu

  :custom
  (corfu-terminal-disable-on-gui)
  (corfu-terminal-resize-minibuffer)
  (corfu-terminal-enable-on-minibuffer)

  :config
  (corfu-terminal-mode 1))

(provide 'elpa-completion-corfu)
;;; elpa-completion-corfu.el ends here
