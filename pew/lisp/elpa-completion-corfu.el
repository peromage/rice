;;; elpa-completion-corfu.el --- Completion by corfu -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimalistic completion framework

;;; Code:
;;; Completion frontend
(use-package corfu
  :demand t

  :bind (:map corfu-map
         ("TAB" . corfu-complete)
         ("<tab>" . corfu-complete)
         ("C-s" . corfu-insert-separator)
         ("C-c" . corfu-insert)
         ("C-k" . corfu-quit)
         ("C-u" . corfu-reset)
         ("RET" . corfu-insert)
         ("<return>" . corfu-insert)
         ("C-j" . pew::corfu::move-to-minibuffer))

  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-cycle nil)
  (corfu-separator ?\s) ;; M-SPC
  (corfu-preview-current 'insert)
  (corfu-preselect 'valid) ;; Quickly submit the first match
  (corfu-on-exact-match 'insert)
  (corfu-quit-at-boundary 'separator) ;; Quit boundary unless separator is used
  (corfu-quit-no-match 'separator) ;; Same above
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-echo-delay '(0.5 . 0.2))

  :config
  (defun pew::corfu::move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  (define-advice pew::terminal-mode-on-init (:after () pew::corfu::in-terminal)
    (setq-local corfu-auto nil))

  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-echo-mode 1)) ;; (use-package corfu)

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
         (lisp-data-mode . pew::cape::elisp-on-init)
         (eshell-mode . pew::cape::eshell-on-init))

  :bind (:map pew::M-c-map
         ("p"  . completion-at-point)
         ("t"  . complete-tag)
         ("d"  . cape-dabbrev)
         ("h"  . cape-history)
         ("f"  . cape-file)
         ("k"  . cape-keyword)
         ("s"  . cape-symbol)
         ("a"  . cape-abbrev)
         ("l"  . cape-line)
         ("w"  . cape-dict)
         ("\\" . cape-tex)
         ("&"  . cape-sgml)
         ("r"  . cape-rfc1345))

  :config
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (setq completion-at-point-functions
        (append (list #'cape-file
                      #'cape-keyword
                      #'cape-dabbrev
                      #'cape-elisp-block
                      ;; #'cape-history
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
    (setq-local completion-at-point-functions (list #'cape-file
                                                    ;; Combined completion style
                                                    (cape-super-capf
                                                     #'elisp-completion-at-point
                                                     #'cape-dabbrev))))

  (defun pew::cape::eshell-on-init ()
    "Set completion style for Eshell mode."
    (setq-local completion-at-point-functions (list #'cape-file
                                                    #'pcomplete-completions-at-point
                                                    (cape-super-capf
                                                     #'elisp-completion-at-point
                                                     #'cape-dabbrev))))) ;; (use-package cape)

(provide 'elpa-completion-corfu)
;;; elpa-completion-corfu.el ends here
