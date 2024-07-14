;;; pewlib-trivial.el --- Common library -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:

;; This file should be loaded as early as possible.

;;; Code:
;;; Paths
(defun pew::normalize-path (base &optional component follow)
  "Normalize path BASE by removing relative representations.
If BASE is a relative path the result will be a path which is relative to the
current path.
When COMPONENT is given it will be appended at the end of BASE.
When FOLLOW is non-nil the result will an absolute path with all symlink
resolved."
  (let ((result (expand-file-name (file-name-concat base component))))
    (if follow (file-truename result) result)))

;;; String functions
(defun pew::file-to-string (path)
  "Read the file content at PATH and return a string.
From: http://xahlee.info/emacs/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun pew::file-to-string-lines (path)
  "Read the file content at PATH and return a list of lines.
From: http://xahlee.info/emacs/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

;;; Number functions
(defun pew::evenp (num)
  "Determine if NUM is odd."
  (zerop (mod num 2)))

(defun pew::oddp (num)
  "Determine if NUM is odd."
  (not (pew::evenp num)))

;;; Data functions
(defun pew::concat (strings &optional separator)
  "Joing a list of STRINGS with SEPARATOR delimited."
  (mapconcat #'identity strings separator))

(defun pew::tolist (x)
  "Wrap input X in a list.
If X is a list already, it is returned as is."
  (if (listp x) x (list x)))

(defun pew::gethash (table &rest keys)
  "Access a hashtable TABLE recursively with a list of KEYS.
This functions is similar to `gethash' but it allows user to specify a list of
keys in one go.
Especially useful when accessing a JSON object."
  (if (= 1 (length keys))
      (gethash (car keys) table)
    (apply #'pew::gethash (gethash (car keys) table) (cdr keys))))

(defmacro pew::swap (a b)
  "Swap values in A and B.
NOTE: A and B must be lvalues."
  `(setq ,a (prog1 ,b (setq ,b ,a))))

(provide 'pewlib-trivial)
;;; pewlib-trivial.el ends here
