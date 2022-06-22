;;; elpa-writing.el --- Writing support -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration used for writing tasks

;;; Code:
;;; Org mode
;; Let `use-package' ensure the latest org package is installed
(use-package org
  :hook (org-mode . pew/text-setup)
  :custom
  ;; Org files
  (org-directory (locate-user-emacs-file "org"))
  (org-default-notes-file (expand-file-name "default-notes.org" org-directory))
  ;; Take every org files under `org-directory'
  (org-agenda-files (list org-directory))

  ;; Startup actions
  (org-startup-indented t)
  (org-startup-folded nil)
  (org-startup-truncated nil)
  (org-startup-numerated nil)
  (org-startup-with-inline-images t)

  ;; Fontify
  (org-src-fontify-natively t)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-emphasized-text t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-block-delimiter-line t)

  ;; Visual
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-hide-macro-markers nil)
  (org-hide-block-startup nil)
  (org-ellipsis " +++")

  ;; Editing
  (org-return-follows-link t)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-insert-heading-respect-content t)
  (org-catch-invisible-edits 'show)
  (org-ctrl-k-protect-subtree t)
  (org-yank-adjusted-subtrees t)
  (org-insert-heading-respect-content t)
  (org-use-fast-todo-selection 'expert) ;; No popup window
  (org-use-fast-tag-selection nil) ;; Always use list selection
  (org-src-preserve-indentation t)

  ;; Log
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; Refile
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Todo
  (org-todo-keywords '((sequence "TODO(t)" "IN_PROGRESS(p)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                       (sequence "ANALYSIS(a!)" "WIP(w!)" "HOLD(h!)" "|" "FIXED(f!)" "WONT_FIX(u@/!)")))
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)

  ;; Capture
  (org-capture-templates
   '(;; Todo
     ("c" "Tasks")
     ("cc" "Create a todo item" entry (file+headline "inbox.org" "Tasks")
      "* TODO %?\nSCHEDULED: %T")
     ("ct" "Create a todo item on a certain day" entry (file+headline "inbox.org" "Tasks")
      "* TODO %?\nSCHEDULED: %^{Pick a date}t")
     ("cn" "Take a note" entry (file+headline "inbox.org" "Notes")
      "* %?\n%i")
     ("cs" "Take a piece of code snippet" entry (file+headline "inbox.org" "Snippets")
      "* %?\n#+begin_src\n%i\n#+end_src")
     ;; Journal
     ("j" "Journal")
     ("jj" "Create a journal" entry (file+olp+datetree "journal.org")
      "* %?\n%U"
      :time-prompt t))))

;;; Markdown mode
(use-package markdown-mode
  :hook (markdown-mode . pew/text-setup))

;;; PlantUML mode
(use-package plantuml-mode
  :custom
  (plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
  (plantuml-default-exec-mode 'jar)
  :mode (("\\.puml\\'" . plantuml-mode)
         ("\\.plantuml\\'" . plantuml-mode))
  :config
  (use-package org
    :defer t
    :ensure nil
    :config
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (add-to-list 'org-babel-load-languages '(plantuml . t)))
  (use-package ob-plantuml
    :defer t
    :ensure nil
    :custom
    (org-plantuml-jar-path plantuml-jar-path)
    (org-plantuml-exec-mode plantuml-default-exec-mode)))

(provide 'elpa-writing)
;;; elpa-writing.el ends here
