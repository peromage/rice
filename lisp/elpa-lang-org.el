;;; elpa-lang-org.el --- Org mode -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for `org-mode' to fit my own taste.

;;; Code:

;; Let `use-package' ensure the latest org package is installed
(use-package org
  :init
  (defun pew/org/setup ()
    "Setup for Org mode."
    (visual-line-mode 1)
    (flyspell-mode 1)
    (electric-pair-mode -1)
    (electric-indent-mode -1))

  ;:hook (org-mode . pew/org/setup)
  :custom
  ;; Org files
  (org-directory (locate-user-emacs-file "org"))
  (org-default-notes-file (expand-file-name "notes.org" org-directory))

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

  ;; Edit
  (org-return-follows-link t)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-insert-heading-respect-content t)
  (org-catch-invisible-edits 'show)
  (org-ctrl-k-protect-subtree t)
  (org-yank-adjusted-subtrees t)
  (org-insert-heading-respect-content t)
  (org-use-fast-todo-selection 'auto)

  ;; Log
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; Refile
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Todo
  (org-todo-keywords '((sequence "TODO(t)" "IN_PROGRESS(p)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                       (sequence "ANALYSIS(a!)" "WIP(w!)" "HOLD(h!)" "|" "FIXED(f!)" "WONT_FIX(u@/!)")))
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t))

(use-package org-capture
  :ensure nil
  :custom
  (org-capture-templates
   '(("t" "Tasks")
     ("tt" "Tasks" entry (file+headline "tasks.org" "All Tasks")
      "* TODO %?\n%T")
     ("tp" "Topic" entry (file+headline "tasks.org" "Topics")
      "* %?"))))

(use-package org-agenda
  :ensure nil
  :custom
  ;; Take every org files under `org-directory'
  (org-agenda-files (list org-directory)))

(provide 'elpa-lang-org)
;;; elpa-lang-org.el ends here
