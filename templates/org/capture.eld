;;; -*- coding: utf-8; mode: lisp-data; -*-

(;; General todo task
 ("t" "New todo item" entry
  (file+olp+datetree "agenda.org")
  "* TODO %?\n"
  :empty-lines 1
  :tree-type month)

 ;; Note taking
 ("n" "New note" plain
  (function (lambda ()
              (let* ((default-directory (expand-file-name "notes" org-directory))
                     (l:prefix (format "%s-" (format-time-string "%Y%m%d%H%M")))
                     (l:suffix ".org")
                     (l:name (read-string "Enter name: " l:prefix)))
                (if (not (string-match-p "\\.org$" l:name))
                    (setq l:name (concat l:name l:suffix)))
                (kill-new (string-trim l:name (regexp-quote l:prefix) (regexp-quote l:suffix)))
                (find-file l:name))))
  "#+title: %c
#+data: %T

%?"
  :kill-buffer t))
