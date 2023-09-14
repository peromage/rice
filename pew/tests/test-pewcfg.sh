#!/usr/bin/env -S emacs -x --batch
;;; test-pewcfg.el --- Test for pewcfg -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:
;; Uint tests for `pewcfg' and its utilities.

;;; Code:
;;; Process arguments
(if (< (length argv) 1) (error "Not enough arguments"))
(setq repo-root-path (nth 0 argv))

;;; Load required files
(add-to-list 'load-path (expand-file-name "pew/lisp" repo-root-path))
(require 'init-common)
(require 'init-pewcfg)

;;; Helpers functions
(defvar test-failure-number 0 "Number of failed tests.")

(defun expect-equal (name a b)
  "Compare A and B and emit error if they don't match.
NAME is used to identify the name of this comparison."
  (declare (indent 1))
  (if (equal a b)
      (message "[ PASSED ] %s" name)
    (setq test-failure-number (1+ test-failure-number))
    (message "[ FAILED ] %s\n>> a: %S\n>> b: %S" name a b)))

(defun expect-expansion (name step expectation form)
  "Expand MACRO and compare the result with EXPECTATION.
NAME is the name of thie comparison.
STEP defines how deep the macro will be expanded.  The value is the same with
the one passed to `pew::expand-macro'."
  (declare (indent 2))
  (expect-equal name expectation (pew::expand-macro form step :result)))

(defun append-pewcfg-keyword-partial-form (keyword form)
  "Append the parameter form to a pewcfg keyword partial form as it is applied."
  `(,@(ensure-list (alist-get keyword pewcfg::keywords)) ,form))

;;; Tests start
(message "[ BEGIN ] Testing for pewcfg")

;;; Test set-custom
(expect-expansion "Test set-custom: Set with three arguments" :all
                  '(customize-set-variable 'foo foovalue "comment")
                  '(pewcfg::set-custom foo foovalue "comment"))

(expect-expansion "Test set-custom: Set without comment" :all
                  '(customize-set-variable 'foo foovalue nil)
                  '(pewcfg::set-custom foo foovalue))
;;; Test set-setq
(expect-expansion "Test set-setq: Set variable" :all
                  '(setq foo foovalue)
                  '(pewcfg::set-setq foo foovalue "comment"))

;;; Test set-setq-default
(expect-expansion "Test set-setq-default: Set variable" 1
                  '(setq-default foo foovalue)
                  '(pewcfg::set-setq-default foo foovalue "comment"))

;;; Test set-bind
(expect-expansion "Test set-bind: Set with definitions" :all
                  '(progn (define-key foo-map "a" #'func1)
                          (define-key foo-map "b" #'func2)
                          foo-map)
                  '(pewcfg::set-bind foo-map
                     ("a" . func1)
                     ("b" . func2)))

(expect-expansion "Test set-bind: Set without definitions" :all
                  '(progn foo-map)
                  '(pewcfg::set-bind foo-map))

;;; Test set-map
(expect-expansion "Test set-map: Set with definitions" 1
                  '(progn (define-prefix-command 'foo-map)
                          (pewcfg::set-bind foo-map
                            ("a" . func1)
                            ("b" . func2)))
                  '(pewcfg::set-map foo-map
                     ("a" . func1)
                     ("b" . func2)))

(expect-expansion "Test set-map: Set without definitions" 1
                  '(progn (define-prefix-command 'foo-map)
                          (pewcfg::set-bind foo-map))
                  '(pewcfg::set-map foo-map))

;;; Test set-transient
(expect-equal "Test set-transient: Set with definitions"
              '(progn (pewcfg::set-map foo-map ("a" . func1) ("b" . func2))
                      (define-key foo-map (kbd "C-h") (lambda () (interactive) (foo :repeat)))
                      (define-key foo-map (kbd "C-g") #'keyboard-quit)
                      (defun foo (arg)))
              (let ((l:expansion (pew::expand-macro '(pewcfg::set-transient foo
                                                       ("a" . func1)
                                                       ("b" . func2))
                                                    1 t)))
                ;; Ignore the function definition
                (setf (nth 4 l:expansion) (seq-take (nth 4 l:expansion) 3))
                l:expansion))

(expect-equal "Test set-transient: Set without definitions"
              '(progn (pewcfg::set-map foo-map)
                      (define-key foo-map (kbd "C-h") (lambda () (interactive) (foo :repeat)))
                      (define-key foo-map (kbd "C-g") #'keyboard-quit)
                      (defun foo (arg)))
              (let ((l:expansion (pew::expand-macro '(pewcfg::set-transient foo)
                                                    1 t)))
                ;; Ignore the function definition
                (setf (nth 4 l:expansion) (seq-take (nth 4 l:expansion) 3))
                l:expansion))

;;; Test set-switch
(expect-equal "Test set-switch: Set with values"
              '(progn (defvar switch::foo '(-1 v1 v2 v3))
                      (defun switch::foo ()))
              (let ((l:expansion (pew::expand-macro '(pewcfg::set-switch foo (v1 v2 v3))
                                                    1 t)))
                ;; Ignore the variable comment
                (setf (nth 1 l:expansion) (butlast (nth 1 l:expansion)))
                ;; Ignore the function definition
                (setf (nth 2 l:expansion) (seq-take (nth 2 l:expansion) 3))
                l:expansion))

(expect-equal "Test set-switch: Set with default values"
              '(progn (defvar switch::foo '(-1 nil t))
                      (defun switch::foo ()))
              (let ((l:expansion (pew::expand-macro '(pewcfg::set-switch foo)
                                                    1 t)))
                ;; Ignore the variable comment
                (setf (nth 1 l:expansion) (butlast (nth 1 l:expansion)))
                ;; Ignore the function definition
                (setf (nth 2 l:expansion) (seq-take (nth 2 l:expansion) 3))
                l:expansion))

;;; Test set-face
(expect-expansion "Test set-face" :all
                  '(set-face-attribute 'foo nil
                                       :family "bar"
                                       :weight 'normal
                                       :height 120
                                       :width 'normal)
                  '(pewcfg::set-face foo
                     :family "bar"
                     :weight normal
                     :height 120
                     :width normal))

;;; Test set-property
(expect-expansion "Test set-property" :all
                  '(progn (put 'foo 'p1 v1)
                          (put 'foo 'p2 v2))
                  '(pewcfg::set-property foo (p1 . v1) (p2 . v2)))

;;; Test set-hook
(expect-expansion "Test set-hook" :all
                  '(add-hook 'foo-hook #'func)
                  '(pewcfg::set-hook foo func))

;;; Test set-automode
(expect-expansion "Test set-automode" :all
                  '(add-to-list 'auto-mode-alist '("matcher regex" . foo-mode))
                  '(pewcfg::set-automode "matcher regex" foo-mode))

;;; Test set-eval
(expect-expansion "Test set-eval" :all
                  '(foo bar)
                  '(pewcfg::set-eval (foo bar)))

;;; Test set-eval-after
(expect-expansion "Test set-eval-after" 1
                  '(with-eval-after-load 'foo (call 911) (call another))
                  '(pewcfg::set-eval-after foo (call 911) (call another)))

;;; Test pewcfg
(expect-expansion "Test pewcfg: Happy path" 1
                  ;; "Not start with a keyword"
                  `(progn :custom ,(append-pewcfg-keyword-partial-form :custom '(foo foovalue "comment")))
                  '(pewcfg :custom (foo foovalue "comment")))

(expect-equal "Test pewcfg: Not start with a keyword"
              ;; "Not start with a keyword"
              '(error "Not start with a keyword")
              (condition-case e
                  (pew::expand-macro '(pewcfg (blah)) 1 t)
                (error e)))

(expect-equal "Test pewcfg: Invalid keyword"
              ;; "Not start with a keyword"
              '(error "Invalid keyword: :customize")
              (condition-case e
                  (pew::expand-macro '(pewcfg :customize (blah)) 1 t)
                (error e)))

;;; Test Utilities
(expect-expansion "Test with-flattened-form" 1
                  '(foo a b c)
                  '(pewcfg::with-flattened-form foo (a b c)))

(expect-expansion "Test with-flattened-cons" 1
                  '(foo a b)
                  '(pewcfg::with-flattened-cons foo (a . b)))

(expect-expansion "Test with-identical-form" 1
                  '(foo (a b c))
                  '(pewcfg::with-identical-form foo (a b c)))

(expect-equal "Test tokey: From string"
              ""
              (pewcfg::tokey "C-c C-c"))

(expect-equal "Test tokey: From vector"
              [tab]
              (pewcfg::tokey [tab]))

(message "[ END ] %s" (if (zerop test-failure-number) "Passed all tests" (format "Failed %d test(s)" test-failure-number)))
(kill-emacs test-failure-number) ;; Exit code is the number of failed tests

(provide 'test-pewcfg)
;;; test-pewcfg.el ends here
