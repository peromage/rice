;;; elpa-org-utils.el --- Org mode supportive packages -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains third party Org mode supportive packages.

;;; Code:
;;; Export backend
;;;; Export for Hugo
(use-package ox-hugo
  :defer t)

;;;; GitHub flavored Markdown
(use-package ox-gfm
  :defer t)

;;; Visual improvement
;;;; Nicer headlines
(use-package org-bullets
  :after org
  :hook (org-mode . pew::org-bullets::oninit)

  :config
  (defun pew::org-bullets::oninit ()
    "`org-bullets' initialization."
    (org-bullets-mode 1)))

(provide 'elpa-org-utils)
;;; elpa-org-utils.el ends here
