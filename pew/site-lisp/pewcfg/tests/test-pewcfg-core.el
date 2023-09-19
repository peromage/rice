;;; test-pewcfg-core.el --- unit tests for pewcfg -*- lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; Test dummies
(add-to-list 'pewcfg::keywords :unittest)

(defun pewcfg::normalize--:unittest (forms)
  forms)

(defun pewcfg::generate--:unittest (&rest args)
  (list args))

(defcustom unittest-var nil "Dummy variable for testing.")

;;; Test suite
(define-test-suite test-pewcfg-core
;;; Test utility functions
  (expect-equal "Test normalize-identity"
    'foo
    (pewcfg::normalize-identity 'foo))

  (expect-equal "Test normalize-pair"
    '(foo bar)
    (pewcfg::normalize-pair '(foo . bar)))

  (expect-equal "Test normalize-first-two"
    '(foo bar)
    (pewcfg::normalize-first-two '(foo bar baz)))

  (expect-equal "Test normalize-single"
    '(foo)
    (pewcfg::normalize-single 'foo))

  (expect-equal "Test until-next-keyword"
    '(:a 4 5 6 :b 7 :c 8 9)
    (pewcfg::until-next-keyword '(1 2 3 :a 4 5 6 :b 7 :c 8 9)))

  (expect-equal "Test until-next-keyword: Empty list"
    nil
    (pewcfg::until-next-keyword nil))

  (expect-equal "Test until-next-keyword: Not a plist"
    nil
    (pewcfg::until-next-keyword '(1 2 3 4 5)))

  (expect-equal "Test slice-keyword-segments"
    '((:a 4 5 6) (:b 7) (:c 8 9))
    (pewcfg::slice-keyword-segments '(1 2 3 :a 4 5 6 :b 7 :c 8 9)))

  (expect-equal "Test slice-keyword-segments: Empty list"
    nil
    (pewcfg::slice-keyword-segments nil))

  (expect-equal "Test slice-keyword-segments: Not a plist"
    nil
    (pewcfg::slice-keyword-segments '(1 2 3 4 5)))

  (expect-equal "Test slice-keyword-segments: Keyword at the end"
    '((:a 4 5 6))
    (pewcfg::slice-keyword-segments '(1 2 3 :a 4 5 6)))

  (expect-equal "Test tokey: From string"
    ""
    (pewcfg::tokey "C-c C-c"))

  (expect-equal "Test tokey: From vector"
    [tab]
    (pewcfg::tokey [tab]))

;;; Test custom theme
  (expect-equal "Test enable-custom-theme: Value set"
    '(foo bar foo bar)
    (progn
      ;; Enable to make the custom theme appear in the `custom-enabled-themes'
      (enable-theme pewcfg::custom-theme)
      (disable-theme pewcfg::custom-theme)
      (setq unittest-var 'foo)
      (let ((l:test-result (list unittest-var)))                               ;; foo
        (custom-theme-set-variables pewcfg::custom-theme '(unittest-var 'bar))
        (pewcfg::enable-custom-theme)
        (push unittest-var l:test-result)                                      ;; bar
        (enable-theme pewcfg::custom-theme)
        (disable-theme pewcfg::custom-theme)
        (push unittest-var l:test-result)                                      ;; foo
        (pewcfg::enable-custom-theme)
        (push unittest-var l:test-result)                                      ;; bar
        (nreverse l:test-result))))

;;; Test keyword application
  (expect-equal "Test apply-keyword: Happy path"
    '((foo foovalue)
      (bar barvalue))
    (pewcfg::apply-keyword :unittest
                           '(foo foovalue)
                           '(bar barvalue)))

  (expect-equal "Test apply-keyword: Invalid keyword"
    '(error "Invalid keyword :foo")
    (condition-case e
        (pewcfg::apply-keyword :foo '())
      (error e)))

;;; Test pewcfg expansion
  (expect-equal "Test pewcfg: Happy path"
    '(progn (foo foovalue) (bar barvalue))
    (macroexpand '(pewcfg
                    :unittest
                    (foo foovalue)
                    (bar barvalue))))

  (expect-equal "Test pewcfg: Not start with a keyword"
    '(error "Not start with a keyword")
    (condition-case e
        (macroexpand '(pewcfg (blah) :unittest (foo foovalue)))
      (error e)))

  (expect-equal "Test pewcfg: Empty body"
    '(error "Not start with a keyword")
    (condition-case e
        (macroexpand '(pewcfg))
      (error e)))

;;; Test :custom
  (expect-equal "Test :custom: Normalize"
    '(('(foo foovalue nil nil "foodoc")
       '(bar barvalue nil nil nil)))
    (pewcfg::normalize--:custom '((foo foovalue "foodoc")
                                  (bar barvalue))))

  (expect-equal "Test :custom: Generate"
    `((custom-theme-set-variables
       ',pewcfg::custom-theme
       '(foo foovalue nil nil "foodoc")
       '(bar barvalue nil nil nil)))
    (pewcfg::generate--:custom `'(foo foovalue nil nil "foodoc")
                               `'(bar barvalue nil nil nil)))

;;; Test :customize
  (expect-equal "Test :customize: Normalize"
    '((foo foovalue "foodoc")
      (bar barvalue "bardoc"))
    (pewcfg::normalize--:customize '((foo foovalue "foodoc")
                                     (bar barvalue "bardoc"))))

  (expect-equal "Test :customize: Generate"
    '((customize-set-variable 'foo foovalue "comment"))
    (pewcfg::generate--:customize 'foo 'foovalue "comment"))

;;; Test :setq
  (expect-equal "Test :setq: Normalize"
    '((foo foovalue bar barvalue))
    (pewcfg::normalize--:setq '((foo foovalue "foodoc")
                                (bar barvalue "bardoc"))))

  (expect-equal "Test :setq: Generate"
    '((setq foo foovalue bar barvalue))
    (pewcfg::generate--:setq 'foo 'foovalue 'bar 'barvalue))

;;; Test :setq-default
  (expect-equal "Test :setq-default: Normalize"
    '((foo foovalue bar barvalue))
    (pewcfg::normalize--:setq '((foo foovalue "foodoc")
                                (bar barvalue "bardoc"))))

  (expect-equal "Test :setq-default: Generate"
    '((setq-default foo foovalue bar barvalue))
    (pewcfg::generate--:setq-default 'foo 'foovalue 'bar 'barvalue))

;;; Test :bind
  (expect-equal "Test :bind: Normalize"
    '((foo-map
       ("a" . func1)
       ("b" . func2)))
    (pewcfg::normalize--:bind '((foo-map
                                 ("a" . func1)
                                 ("b" . func2)))))

  (expect-equal "Test :bind: Generate"
    '((define-key foo-map "a" #'func1)
      (define-key foo-map "b" #'func2)
      foo-map)
    (pewcfg::generate--:bind 'foo-map
      '("a" . func1)
      '("b" . func2)))

  (expect-equal "Test :bind: Generate with no definitions"
    '(foo-map)
    (pewcfg::generate--:bind 'foo-map))

;;; Test :map
  (expect-equal "Test :map: Normalize"
    '((foo-map
       ("a" . func1)
       ("b" . func2)))
    (pewcfg::normalize--:map '((foo-map
                                ("a" . func1)
                                ("b" . func2)))))

  (expect-equal "Test :map: Generate"
    (nconc '((define-prefix-command 'foo-map))
           (pewcfg::generate--:bind 'foo-map
             '("a" . func1)
             '("b" . func2)))
    (pewcfg::generate--:map 'foo-map
      '("a" . func1)
      '("b" . func2)))

;;; Test :transient
  (expect-equal "Test :transient: Normalize"
    '((command
       ("a" . func1)
       ("b" . func2)))
    (pewcfg::normalize--:map '((command
                                ("a" . func1)
                                ("b" . func2)))))

  (expect-equal "Test :transient: Generate"
    (nconc (pewcfg::generate--:map 'command-map
             '("a" . func1)
             '("b" . func2))
           '((define-key command-map (kbd "C-h") (lambda ()))
             (define-key command-map (kbd "C-g") #'keyboard-quit)
             (defun command (arg))))
    (trim-form-recursively (pewcfg::generate--:transient 'command
                             '("a" . func1)
                             '("b" . func2))))

;;; Test :switch
  (expect-equal "Test :switch: Normalize"
    '((foo foovalue)
      (bar barvalue))
    (pewcfg::normalize--:switch '((foo . foovalue)
                                  (bar . barvalue))))

  (expect-equal "Test :switch: Generate"
    '((defvar switch::foo '(-1 v1 v2 v3))
      (defun switch::foo ()))
    (trim-form-recursively (pewcfg::generate--:switch 'foo '(v1 v2 v3))))

  (expect-equal "Test :switch: Generate default"
    '((defvar switch::foo '(-1 t nil))
      (defun switch::foo ()))
    (trim-form-recursively (pewcfg::generate--:switch 'foo)))

;;; Test :face
  (expect-equal "Test :face: Normalize"
    '((foo
       :family "bar"
       :weight normal
       :height 120
       :width normal))
    (pewcfg::normalize--:face '((foo
                                 :family "bar"
                                 :weight normal
                                 :height 120
                                 :width normal))))

  (expect-equal "Test :face: Generate"
    '((set-face-attribute 'foo nil
                          :family "bar"
                          :weight 'normal
                          :height 120
                          :width 'normal))
    (pewcfg::generate--:face 'foo
      :family "bar"
      :weight 'normal
      :height 120
      :width 'normal))

;;; Test :property
  (expect-equal "Test :property: Normalize"
    '((foo (p1 . v1) (p2 . v2)))
    (pewcfg::normalize--:property '((foo (p1 . v1) (p2 . v2)))))

  (expect-equal "Test :property: Generate"
    '((put 'foo 'p1 v1)
      (put 'foo 'p2 v2))
    (pewcfg::generate--:property 'foo '(p1 . v1) '(p2 . v2)))

;;; Test :hook
  (expect-equal "Test :hook: Normalize"
    '((foo-hook func))
    (pewcfg::normalize--:hook '((foo-hook . func))))

  (expect-equal "Test :hook: Generate"
    '((add-hook 'foo-hook #'func))
    (pewcfg::generate--:hook 'foo-hook 'func))

;;; Test :automode
  (expect-equal "Test :automode: Normalize"
    '(("matcher regex" foo-mode))
    (pewcfg::normalize--:automode '(("matcher regex" . foo-mode))))

  (expect-equal "Test :automode: Generate"
    '((add-to-list 'auto-mode-alist '("matcher regex" . foo-mode)))
    (pewcfg::generate--:automode "matcher regex" 'foo-mode))

;;; Test :eval
  (expect-equal "Test :eval: Normalize"
    '(((foo bar)))
    (pewcfg::normalize--:eval '((foo bar))))

  (expect-equal "Test :eval: Generate"
    '((foo bar))
    (pewcfg::generate--:eval '(foo bar)))

;;; Test :eval-after
  (expect-equal "Test :eval-after: Normalize"
    '((foo (bar a) (baz b)))
    (pewcfg::normalize--:eval-after '((foo (bar a) (baz b)))))

  (expect-equal "Test :eval-after: Generate"
    '((with-eval-after-load 'foo (bar a) (baz b)))
    (pewcfg::generate--:eval-after 'foo '(bar a) '(baz b))))

(provide 'test-pewcfg-core)
;;; test-pewcfg-core.el ends here
