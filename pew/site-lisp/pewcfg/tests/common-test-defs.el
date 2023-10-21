;;; common-test-defs.el --- test definitions -*- lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; Helpers functions
(defvar test-labels '(:test-passed "[ PASSED ]"
                      :test-failed "[ FAILED ]"
                      :suite-begin "[ BEGIN ]"
                      :suite-end "[ END ]")
  "A plist defining various labels used in testing.")

(defvar assertion-names '(expect-equal)
  "A list of test assertion function name symbols.")

(defun get-label (keyword)
  "Get KEYWORD values from `test-labels'."
  (plist-get test-labels keyword))

(defun check-assertion (name)
  "Check if a NAME is an assertion function."
  (memq name assertion-names))

(defun expect-equal (name a b)
  "Compare A and B and emit error if they don't match.
NAME is used to identify the name of this comparison."
  (declare (indent 1))
  (if (equal a b)
      (format "%s %s" (get-label :test-passed) name)
    (format "%s %s\n>> a: %S\n>> b: %S" (get-label :test-failed) name a b)))

(defmacro define-test-suite (name &rest body)
  "Define a test suite function to execute.
NAME is a symbol of the suite.
The function will be named with `execute-suite-NAME'."
  (declare (indent 1))
  (let* ((suite-str (symbol-name name))
         (suite-exec (intern (format "execute-suite-%s" suite-str))))
    `(defun ,suite-exec ()
         (let (result)
           (push ,(format "%s %s" (get-label :suite-begin) suite-str) result)
           ,@(mapcar (lambda (b)
                       (if (check-assertion (car b))
                           `(push ,b result)
                         b))
                     body)
           (push ,(format "%s %s" (get-label :suite-end) suite-str) result)
           (nreverse result)))))

(defun execute-test-suites (&rest suites)
  "Execute passed in test SUITES.
To count the number of failures and success, all the assertion functions must
return a string that contains either ':test-passed' or ':test-failed' from
`test-labels'."
  (with-temp-buffer
    (insert (string-join (mapcan (lambda (x) (funcall x)) suites) "\n"))
    (let ((passed-count (how-many (regexp-quote (get-label :test-passed)) (point-min)))
          (failed-count (how-many (regexp-quote (get-label :test-failed)) (point-min))))
      (message (buffer-string))
      (message "Test execution ends\nPassed %d\nFailed %d"
               passed-count
               failed-count)
      failed-count)))

(defun try-take (n lst)
  "Same with `take' for Emacs 28."
  (if (> n (length lst))
      lst
    (butlast lst (- (length lst) n))))

(defun trim-form (form)
  "Simplify a form to make test easier."
  (cond
   ((eq 'defun (car form))
    (try-take 3 form))
   ((eq 'defvar (car form))
    (try-take 3 form))
   ((eq 'lambda (car form))
    (try-take 2 form))
   (t form)))

(defun trim-form-recursively (forms)
  "Like `trim-form' but do it recursively."
  (mapcar (lambda (x)
            (if (or (not (listp x))
                    (null x))
                x
              (let ((trimmed (trim-form x)))
                (if (eq trimmed x)
                    (trim-form-recursively x)
                  trimmed))))
          forms))

(provide 'common-test-defs)
;;; common-test-defs.el ends here
