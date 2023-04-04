;;; -*- coding: utf-8; mode: lisp-data; -*-

(;; Daily tasks
 ("t" "Todo" entry (file+headline "inbox.org" "Tasks")
  "* TODO %?\n")
 ("T" "Todo with Schedule" entry (file+headline "inbox.org" "Tasks")
  "* TODO %?\nSCHEDULED: %^{Pick a date}t")
 ("n" "Note" entry (file+headline "inbox.org" "Notes")
  "* %?\n%i")
 ("s" "Code Snippet" entry (file+headline "inbox.org" "Snippets")
  "* %?\n#+begin_src\n%i\n#+end_src")
 ;; Journal
 ("j" "Create a journal" entry (file+olp+datetree "journal.org")
  "* %?\n%U"
  :time-prompt t))
