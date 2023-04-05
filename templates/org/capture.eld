;;; -*- coding: utf-8; mode: lisp-data; -*-

(;; General todo task
 ("t" "New todo item in agenda" entry
  (file+olp+datetree "agenda.org")
  "* TODO %?\n"
  :empty-lines 1
  :tree-type month)

 ;; Note taking
 ("n" "New note" plain
  (function (lambda ()
              (let* ((l:timestamp (format-time-string "%Y%m%d%H%M-"))
                     (l:file-name (read-string "Enter name: " (concat "notes/" l:timestamp))))
                (if (not (string-match-p "\\.org$" l:file-name))
                    (setq l:file-name (concat l:file-name ".org")))
                (kill-new (string-trim (file-name-base l:file-name) (regexp-quote l:timestamp)))
                ;; https://stackoverflow.com/questions/56274067/how-to-use-the-function-option-of-org-capture-correctly
                (set-buffer (org-capture-target-buffer l:file-name))
                (goto-char (point-max)))))
  "#+title: %c
#+date: %T

%?"
  :kill-buffer t
  ;; Show all content if amending to the existing note file
  :unnarrowed t))
