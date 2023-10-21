;;; pewcfg-use-package.el --- pewcfg with use-package -*- lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:

;; `use-package' is not necessarily needed during the expansions of macros
;; defined in this package.  The caller is advised to install it before using.

;;; Code:

(require 'pewcfg-core)
(require 'use-package nil :noerror)

;;; Helper functions
(defun pewcfg::use-package::translate-pewcfg-keyword (keyword)
  "Translate a KEYWORD used in `pewcfg::use-package' to `pewcfg'.
Currently keywords that start with ':init' or ':config' are checked since
`pewcfg' needs to be evaluated and only makes sense to be put in those blocks.
Return a cons where the car is the `use-package' section keyword and cdr is the
keyword used by `pewcfg' if KEYWORD matches a certain format.
Otherwise return nil."
  (let ((keyword-str (symbol-name keyword)))
    (if (string-match "^:\\(config\\|init\\)/\\(.+\\)" keyword-str)
        (cons (intern (concat ":" (match-string 1 keyword-str)))
              (intern (concat ":" (match-string 2 keyword-str))))
      nil)))

(defmacro pewcfg::use-package (name &rest args)
  "A simple wrapper of `use-package'.
Used like `use-package'.  However, if a keyword starts with ':init/' or
':config/' then it will be translated to use `pewcfg'.
For example:
  (pewcfg::use-package emacs
    :custom
    (foo bar)
    :config/custom
    (fxx bxx))
will be translated to:
  (use-package emacs
    :custom
    (foo bar)
    :config
    (pewcfg
      :custom
      (fxx bxx)))"
  (declare (indent 1))
  `(use-package ,name
     ,@(mapcan (lambda (seg)
                 (let ((matched (pewcfg::use-package::translate-pewcfg-keyword (car seg))))
                   (if matched
                       `(,(car matched) ,(macroexpand `(pewcfg ,(cdr matched) ,@(cdr seg))))
                     seg)))
               (pewcfg::slice-keyword-segments args))))

(defmacro pewcfg::use-package-defer (&rest pkgs)
  "Defer loading a list of PKGS by using `use-package'."
  (declare (indent 0))
  (cons 'progn (mapcar (lambda (x) `(use-package ,x :defer t)) pkgs)))

(defmacro pewcfg::use-package-depend (name &rest args)
  "Declare a dependency relationship of the package NAME.
This simply wrapps around `use-package' with ':ensure' to nil and ':defer' to t.
Usually this is used in one `use-package' form's ':init' or ':config' block to
declare dependent configurations in another package whenever that package is
loaded."
  (declare (indent 1))
  `(use-package ,name :ensure nil :defer t ,@args))

(provide 'pewcfg-use-package)
;;; pewcfg-use-package.el ends here
