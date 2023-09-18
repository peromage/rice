;;; test-pewcfg-use-package.el --- unit tests for pewcfg-use-package -*- lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

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
       (progn (setq ddd 123)
              (setq eee 321))
       :init
       (progn (customize-set-variable 'foo 666 nil)
              (customize-set-variable 'bar 888 nil)))
    (macroexpand-1 '(pewcfg::use-package emacs
                      :custom
                      (aaa val)
                      :config
                      (bbb)
                      (ccc)
                      :config/setq
                      (ddd 123)
                      (eee 321)
                      :init/custom
                      (foo 666)
                      (bar 888))))

  (expect-equal "Test use-package-defer: Normal expand"
    '(progn (use-package foo :defer t)
            (use-package bar :defer t)
            (use-package baz :defer t))
    (macroexpand-1 '(pewcfg::use-package-defer foo bar baz)))

  (expect-equal "Test use-package-depend: Normal expand"
    '(use-package foo
       :ensure nil
       :defer t
       :custom
       (aaa val)
       :config
       (blah))
    (macroexpand-1 '(pewcfg::use-package-depend foo
                      :custom
                      (aaa val)
                      :config
                      (blah)))))

(provide 'test-pewcfg-use-package)
;;; test-pewcfg-use-package.el ends here
