;;; elpa-org.el --- Org writing support -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration used for Org writing tasks.

;;; Code:
;;; Org mode
;; Let `use-package' ensure the latest org package is installed
(use-package org
  :hook ((org-mode . pew::text-mode-on-init)
         (org-babel-after-execute . pew::org::refresh-images))

  :custom
;;;; Visual on startup
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-startup-indented t)
  (org-startup-folded 'nofold)
  (org-startup-truncated nil)
  (org-startup-numerated nil)
  (org-startup-with-inline-images nil)
  (org-hide-block-startup nil)
  ;; Default marker visibility
  (org-ellipsis " ...")
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-hide-macro-markers t)
  (org-link-descriptive t)
  (org-pretty-entities t)

;;;; Image displaying
  (org-display-remote-inline-images 'skip)

;;;; Fontify
  (org-src-fontify-natively t)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-emphasized-text t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-block-delimiter-line t)

;;;; Editing
  (org-return-follows-link nil)
  (org-insert-heading-respect-content t)
  (org-catch-invisible-edits 'smart)
  (org-ctrl-k-protect-subtree t)
  (org-yank-adjusted-subtrees t)
  (org-use-fast-tag-selection nil) ;; Always use list selection
  (org-src-preserve-indentation t)
  (org-refile-targets '((nil :maxlevel . 10)))

;;;; Indentation
  ;; No hard indentation: https://orgmode.org/manual/Hard-indentation.html
  (org-odd-levels-only nil)
  (org-adapt-indentation nil)

;;;; Log
  (org-log-done 'time)
  (org-log-into-drawer t)

;;;; Clock
  (org-clock-into-drawer t)
  (org-clock-out-remove-zero-time-clocks nil)
  (org-clock-clocked-in-display 'mode-line)
  (org-clock-ask-before-exiting t)
  (org-clock-rounding-minutes 0) ;; Keep it precise
  (org-clock-out-when-done t)
  (org-clock-persist nil)

;;;; Refile
  (org-refile-allow-creating-parent-nodes 'confirm)

;;;; Babel
  (org-babel-load-languages '((emacs-lisp . t)
                              (shell . t)))
  ;;(org-confirm-babel-evaluate nil)

;;;; Todo
  (org-use-fast-todo-selection 'expert) ;; No popup window
  ;; Omit selection characters after the first general sequence to let Org
  ;; generate them automatically
  (org-todo-keywords (pew::load-data-file (expand-file-name "todo.eld" pew::org-template-dir)))
  (org-enforce-todo-dependencies nil)
  (org-enforce-todo-checkbox-dependencies nil)

;;;; Agenda and capture
  ;; Org files
  (org-directory pew::default-org-dir)
  (org-default-notes-file (expand-file-name "default-notes.org" org-directory))
  ;; Take every org files under `org-directory'
  (org-agenda-files (list (expand-file-name "agenda.org" org-directory)))
  ;; Templates
  (org-capture-templates (pew::load-data-file (expand-file-name "capture.eld" pew::org-template-dir)))

;;;; Helper functions
  :config
  (defun pew::org::refresh-images ()
    "Redisplay inline images if they exist in the current buffer."
    (interactive)
    (if org-inline-image-overlays
        (org-redisplay-inline-images)))

  (defvar pew::org::marker--hidden t
    "`org-mode' Marker visibility.")

  (defun pew::org::toggle-marker ()
    "Pass SHOW with non-nil to make markers visible or vice versa."
    (interactive)
    (setq pew::org::marker--hidden (not pew::org::marker--hidden))
    ;; Those variables are global
    (setq-default org-hide-emphasis-markers pew::org::marker--hidden)
    (setq-default org-hide-leading-stars pew::org::marker--hidden)
    (setq-default org-hide-macro-markers pew::org::marker--hidden)
    (setq-default org-link-descriptive pew::org::marker--hidden)
    (setq-default org-pretty-entities pew::org::marker--hidden)
    (org-mode-restart))

  (defun pew::org::goto-heading (level &optional to-end)
    "Move cursor to the selected heading in the current `org-mode' buffer.
Minibuffer will show up with the specified LEVEL of headings and move cursor to
it once the choice is confirmed.
If LEVEL is 0, all headings are selectable.
If TO-END is non-nil the cursor will be moved to the end of the heading.
Otherwise the cursor is placed at the beginning of the heading."
    (interactive "nSearch heading level: ")
    (if (not (eq 'org-mode major-mode))
        (message "Not an org buffer.")
      (let* ((l:headings (mapcar (lambda (e) (cons (org-element-property :title e) e))
                                 (seq-filter
                                  (lambda (e)
                                    (if (zerop level) t
                                      (= level (org-element-property :level e))))
                                  (org-map-entries #'org-element-at-point))))
             (l:selected (cdr (assoc
                               (completing-read "Select a heading: " l:headings nil t)
                               l:headings))))
        (goto-char (org-element-property
                    (if to-end :end :begin)
                    l:selected))))))

(provide 'elpa-org)
;;; elpa-org.el ends here
