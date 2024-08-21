;;; test-pewcfg-use-package.el --- unit tests for pewcfg-use-package -*- lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; Test dummy handlers
(add-to-list 'pewcfg::keywords :unittest)

(defun pewcfg::normalize--:unittest (forms)
  forms)

(defun pewcfg::generate--:unittest (&rest args)
  (list args))

;;; Test suite
(define-test-suite test-pewcfg-use-package
  (expect-equal "Test translate-pewcfg-keyword: None matching 1"
    nil
    (pewcfg::use-package::translate-pewcfg-keyword :init))

  (expect-equal "Test translate-pewcfg-keyword: None matching 2"
    nil
    (pewcfg::use-package::translate-pewcfg-keyword :config/))

  (expect-equal "Test translate-pewcfg-keyword: Matched"
    '(:config . :something)
    (pewcfg::use-package::translate-pewcfg-keyword :config/something))

  (expect-equal "Test use-package: Normal expand"
    '(use-package emacs
       :custom
       (aaa val)
       :config
       (bbb)
       (ccc)
       :config
       (progn (ddd 123)
              (eee 321))
       :init
       (progn (foo 666)
              (bar 888)))
    (macroexpand-1 '(pewcfg::use-package emacs
                      :custom
                      (aaa val)
                      :config
                      (bbb)
                      (ccc)
                      :config/unittest
                      (ddd 123)
                      (eee 321)
                      :init/unittest
                      (foo 666)
                      (bar 888))))

  (expect-equal "Test use-package-defer: Normal expand"
    '(progn (use-package foo :defer t)
            (use-package bar :defer t)
            (use-package baz :defer t))
    (macroexpand-1 '(pewcfg::defer-use-packages foo bar baz)))

  (expect-equal "Test use-package-depend: Normal expand"
    '(use-package foo
       :ensure nil
       :defer t
       :custom
       (aaa val)
       :config
       (blah))
    (macroexpand-1 '(pewcfg::use-package-fragment foo
                      :custom
                      (aaa val)
                      :config
                      (blah)))))

(provide 'test-pewcfg-use-package)
;;; test-pewcfg-use-package.el ends here
