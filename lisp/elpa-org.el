;;; elpa-org.el --- Org writing support -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration used for Org writing tasks.

;;; Code:

;;; Org mode
;; Let `use-package' ensure the latest org package is installed
(use-package org
  :init
  (defun pew/org/refresh-images ()
    "Redisplay inline images if they exist in the current buffer."
    (interactive)
    (if org-inline-image-overlays
        (org-redisplay-inline-images)))

  (defun pew/org/marker-visible (&optional show)
    "Pass SHOW with non-nil to make markers visible or vice versa."
    (setq-default org-hide-emphasis-markers (not show))
    (setq-default org-hide-leading-stars (not show))
    (setq-default org-link-descriptive (not show))
    (org-mode-restart))

  (defun pew/org/show-markers ()
    "Interactive command to show markers."
    (interactive)
    (pew/org/marker-visible t))

  (defun pew/org/hide-markers ()
    "Interactive command to hide markers."
    (interactive)
    (pew/org/marker-visible nil))

  :hook ((org-mode . pew/text-common-setup)
         (org-babel-after-execute . pew/org/refresh-images))
  :custom
  ;; Org files
  (org-directory (locate-user-emacs-file "org"))
  (org-default-notes-file (expand-file-name "default-notes.org" org-directory))
  ;; Take every org files under `org-directory'
  (org-agenda-files (list org-directory))

  ;; Visual on startup
  (org-startup-indented t)
  (org-startup-folded 'showeverything)
  (org-startup-truncated nil)
  (org-startup-numerated nil)
  (org-startup-with-inline-images nil)
  (org-hide-emphasis-markers nil)
  (org-hide-leading-stars nil)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-hide-macro-markers nil)
  (org-hide-block-startup nil)
  (org-link-descriptive nil)
  (org-ellipsis " ...")

  ;; Image displaying
  (org-display-remote-inline-images 'skip)

  ;; Fontify
  (org-src-fontify-natively t)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-emphasized-text t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-block-delimiter-line t)

  ;; Editing
  (org-return-follows-link t)
  (org-insert-heading-respect-content t)
  (org-catch-invisible-edits 'show)
  (org-ctrl-k-protect-subtree t)
  (org-yank-adjusted-subtrees t)
  (org-insert-heading-respect-content t)
  (org-use-fast-tag-selection nil) ;; Always use list selection
  (org-src-preserve-indentation t)
  (org-refile-targets '((nil :maxlevel . 10)))
  (org-odd-levels-only nil)

  ;; Log
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; Refile
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Babel
  (org-babel-load-languages '((emacs-lisp . t)
                              (shell . t)))
  ;;(org-confirm-babel-evaluate nil)

  ;; Todo
  (org-use-fast-todo-selection 'expert) ;; No popup window
  ;; Omit selection characters after the first general sequence to let Org
  ;; generate them automatically
  (org-todo-keywords
   '(;; General
     (sequence "TODO(t)" "PROGRESSING(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
     ;; Jira style
     (sequence "ANALYSIS(!)" "DEVELOPMENT(!)" "INTEGRATION(!)" "HOLD(!)" "CP(!)"
               "|"
               "FIXED(!/!)" "DUPLICATE(!/!)" "INVALID(!/!)" "WONT-FIX(@/!)")))
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)

  ;; Capture
  (org-capture-templates
   '(;; Daily tasks
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
      :time-prompt t))))

;;; Export for Hugo
(use-package ox-hugo
  :defer t)

;;; GitHub flavored Markdown
(use-package ox-gfm
  :defer t)

;;; Enable focused view
(use-package visual-fill-column
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-fringes-outside-margins nil))

(provide 'elpa-org)
;;; elpa-org.el ends here
