;;; elpa-completion-corfu.el --- Completion by corfu -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimalistic completion framework

;;; Code:
;;; Completion frontend
(use-package corfu
  :demand t

  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ("S-TAB" . corfu-previous)
         ("<tab>" . corfu-next)
         ("<backtab>" . corfu-previous)
         ("C-s" . corfu-insert-separator)
         ("C-c" . corfu-insert)
         ("C-k" . corfu-quit)
         ("RET" . nil)
         ("<return>" . nil)
         ("C-j" . pew::corfu::move-to-minibuffer))

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
  (corfu-echo-delay '(0.5 . 0.2))

  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode)

  (defun pew::corfu::move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  (define-advice pew::terminal-mode-on-init (:after () pew::corfu::in-terminal)
    (setq-local corfu-auto nil))) ;; (use-package corfu)

;;; Makes corfu usable in terminal
(use-package corfu-terminal
  :after corfu

  :custom
  (corfu-terminal-disable-on-gui t)
  (corfu-terminal-resize-minibuffer t)
  (corfu-terminal-enable-on-minibuffer t)

  :config
  (corfu-terminal-mode 1))

;;; Make it prettier
(use-package kind-icon
  :after corfu

  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Completion backend
(use-package cape
  :demand t
  :after corfu
  :hook ((lisp-interaction-mode . pew::cape::elisp-on-init)
         (emacs-lisp-mode . pew::cape::elisp-on-init)
         (lisp-data-mode . pew::cape::elisp-on-init))

  :bind (:map pew::M-u-map
         ("p" . completion-at-point)
         ("t" . complete-tag)
         ("d" . cape-dabbrev)
         ("h" . cape-history)
         ("f" . cape-file)
         ("k" . cape-keyword)
         ("s" . cape-symbol)
         ("l" . cape-line)
         ("w" . cape-dict))

  :config
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (setq completion-at-point-functions
        (append (list #'cape-dabbrev
                      #'cape-file
                      #'cape-elisp-block
                      ;; #'cape-history
                      ;; #'cape-keyword
                      ;; #'cape-tex
                      ;; #'cape-sgml
                      ;; #'cape-rfc1345
                      ;; #'cape-abbrev
                      ;; #'cape-dict
                      ;; #'cape-symbol
                      ;; #'cape-line
                      )
                completion-at-point-functions))

  (defun pew::cape::elisp-on-init ()
    "Set completion style for ELisp mode."
    (setq-local completion-at-point-functions
                ;; Combined completion style
                (list (cape-super-capf #'elisp-completion-at-point
                                       #'cape-dabbrev))))) ;; (use-package cape)

(provide 'elpa-completion-corfu)
;;; elpa-completion-corfu.el ends here
