;;; test-pewcfg.el --- Test for pewcfg -*- lexical-binding: t; -*-

;;; Commentary:
;; Test `pewcfg' expansion.
;; This is a ELisp script and it should be executed with "emacs --script".

;;; Code:
(progn
;;; Test helpers
  (defun expect-equal (name a b)
    "Compare A and B and emit error if they don't match.
NAME is used to identify the name of this comparison."
    (declare (indent 1))
    (if (equal a b)
        (message "[ PASSED ] %s" name)
      (error "[ FAILED ] %s" name)))

  (defmacro expect-expansion (name step expectation macro)
    "Expand MACRO and compare the result with EXPECTATION.
NAME is the name of thie comparison.
STEP defines how deep the macro will be expanded.  The value is the same with
the one passed to `pew::expand-macro'."
    (declare (indent 2))
    `(expect-equal ,name
       ,expectation
       (pew::expand-macro ,macro ,step :result)))

;;; Tests start
  (message "[ BEGIN ] Testing for pewcfg")

;;; :custom
  (expect-expansion "set-custom" :all
    '(customize-set-variable 'foo foovalue "comment")
    (pewcfg::set-custom foo foovalue "comment"))

  (expect-expansion "set-custom: No comment" :all
    '(customize-set-variable 'foo foovalue nil)
    (pewcfg::set-custom foo foovalue))

;;; :bind
  (expect-expansion "set-bind" :all
    '(progn (define-key foo-map "a" #'func1)
            (define-key foo-map "b" #'func2)
            foo-map)
    (pewcfg::set-bind foo-map
                      ("a" . func1)
                      ("b" . func2)))

  (expect-expansion "set-bind: No definitions" :all
    '(progn foo-map)
    (pewcfg::set-bind foo-map))

;;; :map
  (expect-expansion "set-map" 1
    '(progn (define-prefix-command 'foo-map)
            (pewcfg::set-bind foo-map
                              ("a" . func1)
                              ("b" . func2)))
    (pewcfg::set-map foo-map
                     ("a" . func1)
                     ("b" . func2)))

  (expect-expansion "set-map: No definitions" 1
    '(progn (define-prefix-command 'foo-map)
            (pewcfg::set-bind foo-map))
    (pewcfg::set-map foo-map))

;;; :transient
  (expect-equal "set-transient"
    '(progn (pewcfg::set-map foo-map ("a" . func1) ("b" . func2))
            (define-key foo-map (kbd "C-h") (lambda () (interactive) (foo :repeat)))
            (define-key foo-map (kbd "C-g") #'keyboard-quit)
            (defun foo (arg)))
    (let ((l:expansion (pew::expand-macro (pewcfg::set-transient foo
                                                                 ("a" . func1)
                                                                 ("b" . func2))
                                          1 t)))
      ;; Ignore the function definition
      (setf (nth 4 l:expansion) (seq-take (nth 4 l:expansion) 3))
      l:expansion))

  (expect-equal "set-transient: No definitions"
    '(progn (pewcfg::set-map foo-map)
            (define-key foo-map (kbd "C-h") (lambda () (interactive) (foo :repeat)))
            (define-key foo-map (kbd "C-g") #'keyboard-quit)
            (defun foo (arg)))
    (let ((l:expansion (pew::expand-macro (pewcfg::set-transient foo)
                                          1 t)))
      ;; Ignore the function definition
      (setf (nth 4 l:expansion) (seq-take (nth 4 l:expansion) 3))
      l:expansion))

;;; :switch
  (expect-equal "set-switch"
    '(progn (defvar switch/foo '(-1 v1 v2 v3))
            (defun switch/foo ()))
    (let ((l:expansion (pew::expand-macro (pewcfg::set-switch foo (v1 v2 v3))
                                          1 t)))
      ;; Ignore the variable comment
      (setf (nth 1 l:expansion) (butlast (nth 1 l:expansion)))
      ;; Ignore the function definition
      (setf (nth 2 l:expansion) (seq-take (nth 2 l:expansion) 3))
      l:expansion))

  (expect-equal "set-switch: Default values"
    '(progn (defvar switch/foo '(-1 nil t))
            (defun switch/foo ()))
    (let ((l:expansion (pew::expand-macro (pewcfg::set-switch foo)
                                          1 t)))
      ;; Ignore the variable comment
      (setf (nth 1 l:expansion) (butlast (nth 1 l:expansion)))
      ;; Ignore the function definition
      (setf (nth 2 l:expansion) (seq-take (nth 2 l:expansion) 3))
      l:expansion))

;;; :face
  (expect-expansion "set-face" :all
    '(set-face-attribute 'foo nil
                         :family "bar"
                         :weight 'normal
                         :height 120
                         :width 'normal)
    (pewcfg::set-face foo
                      :family "bar"
                      :weight normal
                      :height 120
                      :width normal))

  (expect-expansion "set-property" :all
    '(progn (put 'foo 'p1 v1)
            (put 'foo 'p2 v2))
    (pewcfg::set-property foo (p1 . v1) (p2 . v2)))

  (expect-expansion "set-hook" :all
    '(add-hook 'foo-hook #'func)
    (pewcfg::set-hook foo func))

  (expect-expansion "set-automode" :all
    '(add-to-list 'auto-mode-alist '("matcher regex" . foo-mode))
    (pewcfg::set-automode "matcher regex" foo-mode))

  (expect-expansion "set-eval" :all
    '(foo bar)
    (pewcfg::set-eval (foo bar)))

  (expect-expansion "set-eval-after" 1
    '(with-eval-after-load 'foo (call 911) (call another))
    (pewcfg::set-eval-after foo (call 911) (call another)))

;;; Utilities
  (expect-expansion "with-flattened-form" 1
    '(foo a b c)
    (pewcfg::with-flattened-form foo (a b c)))

  (expect-expansion "with-flattened-cons" 1
    '(foo a b)
    (pewcfg::with-flattened-cons foo (a . b)))

  (expect-expansion "with-identical-form" 1
    '(foo (a b c))
    (pewcfg::with-identical-form foo (a b c)))

  (expect "tokey: string"
    ""
    (pewcfg::tokey "C-c C-c"))

  (expect "tokey: vector"
    [tab]
    (pewcfg::tokey [tab]))

  (message "[ END ] Passed all"))
