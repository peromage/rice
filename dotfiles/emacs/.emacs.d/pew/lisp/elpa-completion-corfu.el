;;; elpa-completion-corfu.el --- corfu and complementary -*- lexical-binding: t; -*-
;;; Commentary:

;; Minimalistic completion framework.

;;; Code:

(use-package corfu
  :ensure t
  :demand t
  :bind ( :map corfu-map
          ("TAB"      . corfu-complete)
          ("<tab>"    . corfu-complete)
          ("RET"      . corfu-insert)
          ("<return>" . corfu-insert)
          ("C-j"      . corfu-insert)
          ("C-c"      . corfu-insert)
          ("C-s"      . corfu-insert-separator)
          ("C-k"      . corfu-reset)
          ("M-m"      . pew::corfu::move-to-minibuffer) )
  :custom
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

  :config
  (defun pew::corfu::move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

  (define-advice pewlib::editor::as-terminal-mode (:after () pew::corfu::in-terminal)
    (setq-local corfu-auto nil))

  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-echo-mode 1)) ;; End corfu

(use-package corfu-terminal
  :ensure t
  :after corfu
  :custom
  (corfu-terminal-disable-on-gui t)
  (corfu-terminal-resize-minibuffer nil)
  (corfu-terminal-enable-on-minibuffer nil)
  :config
  (corfu-terminal-mode 1))

(use-package cape
  :ensure t
  :after corfu
  :hook ((lisp-interaction-mode . pew::cape::on-elisp-mode)
         (emacs-lisp-mode . pew::cape::on-elisp-mode)
         (lisp-data-mode . pew::cape::on-elisp-mode)
         (eshell-mode . pew::cape::on-eshell-mode))
  :bind ( :map pew::M-c-map
          ("i"  . completion-at-point)
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
          ("r"  . cape-rfc1345) )

  :preface
  (defun pew::cape::on-elisp-mode ()
    "Set completion style for ELisp mode."
    (setq-local completion-at-point-functions (list #'cape-file
                                                    ;; Combined completion style
                                                    (cape-capf-super
                                                     #'elisp-completion-at-point
                                                     #'cape-dabbrev))))

  (defun pew::cape::on-eshell-mode ()
    "Set completion style for Eshell mode."
    (setq-local completion-at-point-functions (list #'cape-file
                                                    #'pcomplete-completions-at-point
                                                    (cape-capf-super
                                                     #'elisp-completion-at-point
                                                     #'cape-dabbrev))))

  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; The order matters!
  ;; NOTE: This setq doesn't work in :config for some reason.
  (setq completion-at-point-functions (append (list #'cape-file
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
                                              completion-at-point-functions))) ;; End cape

(provide 'elpa-completion-corfu)
;;; elpa-completion-corfu.el ends here
