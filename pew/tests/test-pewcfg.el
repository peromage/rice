;;; test-pewcfg.el --- Test for pewcfg -*- lexical-binding: t; -*-

;;; Commentary:
;; Test `pewcfg' expansion.
;; This is a ELisp script and it should be executed with "emacs --script".

;;; Code:
(progn
;;; Test helpers
  (defun expect (name a b)
    "Compare A and B and emit error if they don't match.
NAME is used to identify the name of this comparison."
    (if (equal a b)
        (message "[ PASSED ] %s" name)
      (error "[ FAILED ] %s" name)))

  (defmacro expect-expansion (name step macro expection)
    "Expand MACRO and compare the result with EXPECTION.
NAME is the name of thie comparison.
STEP defines how deep the macro will be expanded.  The value is the same with
the one passed to `pew::expand-macro'."
    `(expect ,name
             (pew::expand-macro ,macro ,step :result)
             ,expection))


;;; Tests start
  (message "[ BEGIN ] Testing for pewcfg")

  (expect-expansion
   "set-custom" :all
   (pewcfg::set-custom foo foovalue "comment")
   '(customize-set-variable 'foo foovalue "comment"))

  (expect-expansion
   "set-custom: No comment" :all
   (pewcfg::set-custom foo foovalue)
   '(customize-set-variable 'foo foovalue nil))

  (expect-expansion
   "set-bind" :all
   (pewcfg::set-bind foo-map
                     ("a" . func1)
                     ("b" . func2))
   '(progn (define-key foo-map "a" #'func1)
           (define-key foo-map "b" #'func2)
           foo-map))

  (expect-expansion
   "set-bind: No definitions" :all
   (pewcfg::set-bind foo-map)
   '(progn foo-map))

  (expect-expansion
   "set-map" 1
   (pewcfg::set-map foo-map
                    ("a" . func1)
                    ("b" . func2))
   '(progn (define-prefix-command 'foo-map)
           (pewcfg::set-bind foo-map
                             ("a" . func1)
                             ("b" . func2))))

  (expect-expansion
   "set-map: No definitions" 1
   (pewcfg::set-map foo-map)
   '(progn (define-prefix-command 'foo-map)
           (pewcfg::set-bind foo-map)))

  (expect
   "set-transient"
   (let ((l:expansion (pew::expand-macro (pewcfg::set-transient foo
                                                                ("a" . func1)
                                                                ("b" . func2))
                                         1 t)))
     ;; Ignore the function definition
     (setf (nth 4 l:expansion) (seq-take (nth 4 l:expansion) 3))
     l:expansion)
   '(progn (pewcfg::set-map foo-map ("a" . func1) ("b" . func2))
           (define-key foo-map (kbd "C-h") (lambda () (interactive) (foo :repeat)))
           (define-key foo-map (kbd "C-g") #'keyboard-quit)
           (defun foo (arg))))

  (expect
   "set-transient: No definitions"
   (let ((l:expansion (pew::expand-macro (pewcfg::set-transient foo)
                                         1 t)))
     ;; Ignore the function definition
     (setf (nth 4 l:expansion) (seq-take (nth 4 l:expansion) 3))
     l:expansion)
   '(progn (pewcfg::set-map foo-map)
           (define-key foo-map (kbd "C-h") (lambda () (interactive) (foo :repeat)))
           (define-key foo-map (kbd "C-g") #'keyboard-quit)
           (defun foo (arg))))

  (expect
   "set-switch"
   (let ((l:expansion (pew::expand-macro (pewcfg::set-switch foo
                                                             (v1 v2 v3))
                                         1 t)))
     ;; Ignore the variable comment
     (setf (nth 1 l:expansion) (butlast (nth 1 l:expansion)))
     ;; Ignore the function definition
     (setf (nth 2 l:expansion) (seq-take (nth 2 l:expansion) 3))
     l:expansion)
   '(progn (defvar switch/foo '(-1 v1 v2 v3))
           (defun switch/foo ())))

  (expect
   "set-switch: Default values"
   (let ((l:expansion (pew::expand-macro (pewcfg::set-switch foo)
                                         1 t)))
     ;; Ignore the variable comment
     (setf (nth 1 l:expansion) (butlast (nth 1 l:expansion)))
     ;; Ignore the function definition
     (setf (nth 2 l:expansion) (seq-take (nth 2 l:expansion) 3))
     l:expansion)
   '(progn (defvar switch/foo '(-1 nil t))
           (defun switch/foo ())))

  (expect-expansion
   "set-face" :all
   (pewcfg::set-face foo
                     :family "bar"
                     :weight normal
                     :height 120
                     :width normal)
   '(set-face-attribute 'foo nil
                        :family "bar"
                        :weight 'normal
                        :height 120
                        :width 'normal))

  (expect-expansion
   "set-property" :all
   (pewcfg::set-property foo (p1 . v1) (p2 . v2))
   '(progn (put 'foo 'p1 v1)
           (put 'foo 'p2 v2)))

  (expect-expansion
   "set-hook" :all
   (pewcfg::set-hook foo func)
   '(add-hook 'foo-hook #'func))

  (expect-expansion
   "set-automode" :all
   (pewcfg::set-automode "matcher regex" foo-mode)
   '(add-to-list 'auto-mode-alist '("matcher regex" . foo-mode)))

  (expect-expansion
   "set-eval" :all
   (pewcfg::set-eval (foo bar))
   '(foo bar))

  (expect-expansion
   "set-eval-after" 1
   (pewcfg::set-eval-after foo (call 911) (call another))
   '(with-eval-after-load 'foo (call 911) (call another)))

  (expect
   "Tokey key string"
   (pewcfg::tokey "C-c C-c")
   "")

  (expect
   "Tokey key vector"
   (pewcfg::tokey [tab])
   [tab])

  (message "[ END ] Passed all"))
