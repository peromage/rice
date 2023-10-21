;;; elpa-org.el --- org mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Package: org
;; Let `use-package' ensure the latest org package to be installed
(pewcfg::use-package org
  :commands org-mode
  :hook ((org-mode . pew::org::oninit)
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
  (org-hide-emphasis-markers pew::org::marker--hidden)
  (org-hide-leading-stars pew::org::marker--hidden)
  (org-hide-macro-markers pew::org::marker--hidden)
  (org-link-descriptive pew::org::marker--hidden)
  (org-pretty-entities pew::org::marker--hidden)

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
  (org-fold-catch-invisible-edits 'smart)
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
  (org-confirm-babel-evaluate nil)

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

  :init
  (defvar pew::org::marker--hidden nil
    "`org-mode' marker visibility.")

  :config
  (defun pew::org::oninit ()
    "Org mode initial setup."
    (pew::text-mode-oninit))

  (defun pew::org::refresh-images ()
    "Redisplay inline images if they exist in the current buffer."
    (interactive)
    (if org-inline-image-overlays
        (org-redisplay-inline-images)))

  (defun pew::org::toggle-marker (&optional show no-restart)
    "Pass SHOW with 1 or -1 to show or hide markers or anything else to toggle.
Non-nil NO-RESTART to suppress `org-mode-restart'."
    (interactive)
    (setq pew::org::marker--hidden (pcase show
                                     (1 nil)
                                     (-1 t)
                                     (_  (not pew::org::marker--hidden))))
    ;; Those variables are global
    (setq-default org-hide-emphasis-markers pew::org::marker--hidden
                  org-hide-leading-stars pew::org::marker--hidden
                  org-hide-macro-markers pew::org::marker--hidden
                  org-link-descriptive pew::org::marker--hidden
                  org-pretty-entities pew::org::marker--hidden)
    (unless no-restart (org-mode-restart)))

  (defun pew::org::goto-heading (level &optional to-end)
    "Move cursor to the selected heading in the current `org-mode' buffer.
Minibuffer will show up with the specified LEVEL of headings and move cursor to
it once the choice is confirmed.
Level can be the following values:
  - 0: All the headings are selectable.
  - nil: Same with 0.
  - non-zero number: Filter this level of headings.
  - non-nil object: Interactively enter level number.
If TO-END is non-nil the cursor will be moved to the end of the heading.
Otherwise the cursor is placed at the beginning of the heading."
    (interactive "P")
    (if (not (eq 'org-mode major-mode))
        (message "Not an org buffer.")
      (setq level (cond ((null level) 0)
                        ((not (numberp level)) (read-number "Search heading level: "))
                        (t level)))
      (let* ((headings (mapcar (lambda (e) (cons (org-element-property :title e) e))
                               (seq-filter
                                (if (zerop level) #'identity
                                  (lambda (e) (= level (org-element-property :level e))))
                                (org-map-entries #'org-element-at-point))))
             (selected (cdr (assoc
                             (completing-read "Select a heading: " headings nil t)
                             headings))))
        ;; When used in `org-capture-templateas', `narrow-to-region' can be used
        ;; together with `:unnarrowed' to resume from existing entries.
        (goto-char (org-element-property
                    (if to-end :end :begin)
                    selected)))))

  (defun pew::org::find-file ()
    "Find files under `org-directory'."
    (interactive)
    (let ((default-directory (file-name-as-directory org-directory)))
      (call-interactively #'find-file)))

  (defun pew::org::add-src-lang-modes (alist)
    "Add modes defined in ALIST to `org-src-lang-modes'.
Duplicated pairs will be removed."
    (mapc (lambda (x) (assoc-delete-all (car x) org-src-lang-modes)) alist)
    (setq org-src-lang-modes (nconc alist org-src-lang-modes)))

  (defun pew::org::add-babel-load-languages (alist)
    "Add languages defined in ALIST to `org-babel-load-languages'.
`org-babel-do-load-languages' will be called underneath."
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (nconc alist org-babel-load-languages)))

  :config/setq
  (org-babel-default-header-args '((:session . "none")
                                   (:results . "output replace")
                                   ;; (:wrap . "example") ;; Might be problematic for pictures
                                   (:exports . "code")
                                   (:cache . "no")
                                   (:noweb . "yes")
                                   (:hlines . "no")
                                   (:tangle . "no")))
  (org-babel-default-inline-header-args '((:session . "none")
                                          (:results . "output replace")
                                          (:exports . "results")
                                          (:cache . "no")
                                          (:noweb . "yes")
                                          (:hlines . "no")
                                          (:tangle . "no")))) ;; End org

;;; Package: org-temp -- Org built-in for template expansion
(pewcfg::use-package org-tempo
  :ensure nil
  :after org)

;;; Package: org-bullets -- Nice headings
(pewcfg::use-package org-bullets
  :after org
  :hook (org-mode . pew::org-bullets::oninit)

  :config
  (defun pew::org-bullets::oninit ()
    "`org-bullets' initialization."
    (org-bullets-mode 1)))

(pewcfg::use-package-defer
  ;; Export backend for Hugo
  ox-hugo
  ;; Export backend for GitHub flavored Markdown
  ox-gfm)

(provide 'elpa-org)
;;; elpa-org.el ends here
