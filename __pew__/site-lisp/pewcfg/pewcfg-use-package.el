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

;;; Facility macros
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

(defmacro pewcfg::use-package-defer-list (&rest names)
  "Defer loading a list of packages in NAMES with `use-package'."
  (declare (indent 0))
  (cons 'progn (mapcar (lambda (x) `(use-package ,x :defer t)) names)))

(defmacro pewcfg::use-package-fragment (name &rest args)
  "Declare a fragment of package NAME in `use-package' form.
This implies `:ensure' nil and `:defer' t so that the fragment declaration is
only effective when the original `use-package' is loaded.
This mostly used in a `use-package' context where it has configuration for
another `use-package' form.
The ARGS is the same with normal `use-package'."
  (declare (indent 1))
  `(use-package ,name :ensure nil :defer t ,@args))

;;; Facility functions
(defun pewcfg::vc-install (repo rev &optional fetcher name backend)
  "Install package from a VC source.
This is a wrapper of `package-vc-install'.
REPO is the name of the repository including owner, e.g. \"peromage/rice\".
FETCHER is the remote where to get the package.  Default to \"github\".
NAME, REV, BACKEND are the specs described in `package-vc-selected-packages'.

NOTE: The package can't be upgraded along with normal package update.  The old
package must be deleted before installing a new version, assuming the REV is not
pinned to a specific revision.

See: https://tony-zorman.com/posts/package-vc-install.html"
  (let ((url (format "https://www.%s.com/%s" (or fetcher "github") repo))
        (name (or name (intern (file-name-base repo))))
        (backend (or backend 'Git)))
    (unless (package-installed-p name)
      (package-vc-install (list name :url url :branch rev :vc-backend backend)))))

(provide 'pewcfg-use-package)
;;; pewcfg-use-package.el ends here
