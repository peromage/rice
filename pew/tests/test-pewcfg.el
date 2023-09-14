;;; test-pewcfg.el --- Test for pewcfg -*- lexical-binding: t; -*-

;;; Commentary:
;; This is a ELisp script and it should be executed with "emacs --script".
;;
;; Test `pewcfg' expansion.

;;; Code:
;;; Load required files
(if (< (length argv) 1)
    (error "Not enough arguments"))

(setq target-lisp-path (nth 0 argv))

(add-to-list 'load-path target-lisp-path)
(require 'init-common)
(require 'init-pewcfg)

;;; Helpers functions
(defun expect-equal (name a b)
  "Compare A and B and emit error if they don't match.
NAME is used to identify the name of this comparison."
  (declare (indent 1))
  (if (equal a b)
      (message "[ PASSED ] %s" name)
    (error "[ FAILED ] %s" name)))

(defun expect-expansion (name step expectation form)
  "Expand MACRO and compare the result with EXPECTATION.
NAME is the name of thie comparison.
STEP defines how deep the macro will be expanded.  The value is the same with
the one passed to `pew::expand-macro'."
  (declare (indent 2))
  (expect-equal name expectation (pew::expand-macro form step :result)))

(defun expand-pewcfg-keyword (keyword form)
  "Expand pewcfg keyword form as it is applied."
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
(let ((l:set-custom-form '(foo foovalue "comment"))
      (l:set-bind-form '(foo-map ("a" . func1)))
      (l:set-map-form '(foo-map ("a" . func1)))
      (l:set-transient-form '(foo ("a" . func1)))
      (l:set-switch-form '(foo . (v1 v2)))
      (l:set-face-form '(foo :family "bar" :height 123))
      (l:set-property-form '(foo (prop val)))
      (l:set-hook-form '(foo . func))
      (l:set-automode-form '("matcher" . mode))
      (l:set-eval-form '(foo bar))
      (l:set-eval-after-form '(foo (call something))))
  (expect-expansion "Test pewcfg: Happy path" 1
                    `(progn
                       :custom
                       ,(expand-pewcfg-keyword :custom l:set-custom-form)
                       :bind
                       ,(expand-pewcfg-keyword :bind l:set-bind-form)
                       :map
                       ,(expand-pewcfg-keyword :map l:set-map-form)
                       :transient
                       ,(expand-pewcfg-keyword :transient l:set-transient-form)
                       :switch
                       ,(expand-pewcfg-keyword :switch l:set-switch-form)
                       :face
                       ,(expand-pewcfg-keyword :face l:set-face-form)
                       :property
                       ,(expand-pewcfg-keyword :property l:set-property-form)
                       :hook
                       ,(expand-pewcfg-keyword :hook l:set-hook-form)
                       :automode
                       ,(expand-pewcfg-keyword :automode l:set-automode-form)
                       :eval
                       ,(expand-pewcfg-keyword :eval l:set-eval-form)
                       :eval-after
                       ,(expand-pewcfg-keyword :eval-after l:set-eval-after-form))
                    `(pewcfg
                       :custom
                       ,l:set-custom-form
                       :bind
                       ,l:set-bind-form
                       :map
                       ,l:set-map-form
                       :transient
                       ,l:set-transient-form
                       :switch
                       ,l:set-switch-form
                       :face
                       ,l:set-face-form
                       :property
                       ,l:set-property-form
                       :hook
                       ,l:set-hook-form
                       :automode
                       ,l:set-automode-form
                       :eval
                       ,l:set-eval-form
                       :eval-after
                       ,l:set-eval-after-form)))

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

(message "[ END ] Passed all")

(provide 'test-pewcfg)
;;; test-pewcfg.el ends here
