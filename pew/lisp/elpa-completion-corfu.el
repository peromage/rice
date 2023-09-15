;;; elpa-completion-corfu.el --- corfu and complementary -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimalistic completion framework
;;; Code:

;;; Package: corfu -- Completion frontend
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

  :config
  (pewcfg
    :setq
    (corfu-auto t)
    (corfu-auto-prefix 2)
    (corfu-cycle nil)
    (corfu-separator ?\s) ;; M-SPC
    (corfu-preview-current 'insert)
    (corfu-preselect 'prompt) ;; Do not select automatically
    (corfu-on-exact-match 'insert)
    (corfu-quit-at-boundary 'separator) ;; Quit boundary unless separator is used
    (corfu-quit-no-match 'separator) ;; Same above
    (corfu-popupinfo-delay '(0.5 . 0.2))
    (corfu-echo-delay '(0.5 . 0.2))

    :eval
    (defun pew::corfu::move-to-minibuffer ()
      (interactive)
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data)))

    (define-advice pew::terminal-mode-oninit (:after () pew::corfu::in-terminal)
      (setq-local corfu-auto nil))

    :eval
    (global-corfu-mode 1)
    (corfu-history-mode 1)
    (corfu-popupinfo-mode 1)
    (corfu-echo-mode 1))) ;; End corfu

;;; Package: corfu-terminal -- Makes corfu usable in terminal
(use-package corfu-terminal
  :after corfu
  :config
  (pewcfg
    :setq
    (corfu-terminal-disable-on-gui t)
    (corfu-terminal-resize-minibuffer t)
    (corfu-terminal-enable-on-minibuffer t)

    :eval
    (corfu-terminal-mode 1)))

;;; Package: kind-icon -- Make corfu prettier
(use-package kind-icon
  :after corfu
  :config
  (pewcfg
    :setq
    (kind-icon-default-face 'corfu-default) ;; To compute blended backgrounds correctly

    :eval
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

;;; Package: cape -- Completion backend
(use-package cape
  :demand t
  :after corfu
  :hook ((lisp-interaction-mode . pew::cape::elisp-oninit)
         (emacs-lisp-mode . pew::cape::elisp-oninit)
         (lisp-data-mode . pew::cape::elisp-oninit)
         (eshell-mode . pew::cape::eshell-oninit))

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
  (pewcfg
    :setq
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    ;; NOTE: The order matters!
    (completion-at-point-functions (append (list #'cape-file
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

    :eval
    (defun pew::cape::elisp-oninit ()
      "Set completion style for ELisp mode."
      (setq-local completion-at-point-functions (list #'cape-file
                                                      ;; Combined completion style
                                                      (cape-super-capf
                                                       #'elisp-completion-at-point
                                                       #'cape-dabbrev))))

    (defun pew::cape::eshell-oninit ()
      "Set completion style for Eshell mode."
      (setq-local completion-at-point-functions (list #'cape-file
                                                      #'pcomplete-completions-at-point
                                                      (cape-super-capf
                                                       #'elisp-completion-at-point
                                                       #'cape-dabbrev)))))) ;; End cape

(provide 'elpa-completion-corfu)
;;; elpa-completion-corfu.el ends here
