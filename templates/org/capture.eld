;;; -*- coding: utf-8; mode: lisp-data; -*-

(;; General todo task
 ("t" "New todo item" entry
  (file+olp+datetree "agenda.org")
  "* TODO %?\n"
  :empty-lines 1
  :tree-type month)

 ;; Note taking
 ("n" "New note" plain
  (function (lambda () (find-file
                        (expand-file-name
                         (format "notes/%s-%s.org"
                                 (format-time-string "%Y%m%d%H%M")
                                 (read-string "topic: "))
                         org-directory))))
  "%?"
  :kill-buffer t))
