;;; pewcfg-use-package.el --- pewcfg with use-package -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Helper functions
(defun pewcfg::use-package::translate-pewcfg-keyword (keyword)
  "Translate a KEYWORD used in `pewcfg::use-package' to `pewcfg'.
Currently keywords that start with ':config' or ':init' are checked since
`pewcfg' needs to be evaluated and only makes sense to be put in those blocks.
Return a cons where the car is the `use-package' section keyword and cdr is the
keyword used by `pewcfg' if KEYWORD matches a certain format.
Otherwise return nil."
  (let ((l:keyword-str (symbol-name keyword)))
    (if (string-match "^\\(:config\\|:init\\)/\\(.+\\)" l:keyword-str)
        (cons (intern (match-string 1 l:keyword-str))
              (intern (match-string 2 l:keyword-str)))
      nil)))

(defmacro pewcfg::use-package (name &rest args)
  (mapcan (lambda (seg)
            (let ((l:matched (pewcfg::use-package::translate-pewcfg-keyword (car seg))))
              (if l:matched
                  `(,(car l:matched) ,(macroexpand `(pewcfg ,(cdr l:matched) ,@(cdr seg))))
                seg)))
          (pewcfg::slice-keyword-segments args)))

(provide 'pewcfg-use-package)
;;; pewcfg-use-package.el ends here
