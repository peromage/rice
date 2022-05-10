;;; elpa-lang-org.el --- Org mode -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for `org-mode' to fit my own taste.

;;; Code:

(defun pew/org/setup ()
  "Setup for Org mode."
  (org-indent-mode 1)
  (visual-line-mode 1)
  (electric-pair-mode -1)
  (electric-indent-mode -1))

(use-package org
  :ensure nil
  :hook (org-mode . pew/org/setup)
  :custom
  (org-directory (locate-user-emacs-file "org"))
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-agenda-files (expand-file-name "agenda.org" org-directory))
  (org-startup-truncated nil)
  (org-startup-with-inline-images t)
  (org-startup-folded nil)
  (org-src-fontify-natively t)
  (org-return-follows-link t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-indent-mode-turns-on-hiding-stars nil)

  ;; Todo
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)"))))

(provide 'elpa-lang-org)
;;; elpa-lang-org.el ends here
