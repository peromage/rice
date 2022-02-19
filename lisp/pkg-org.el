;;; pkg-org.el --- Org mode tweaks -*- lexical-binding: t -*-
;;; Commentary:
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
  (org-src-fontify-natively t)
  (org-startup-folded nil)
  (org-return-follows-link t)
  (org-startup-truncated nil)
  (org-startup-with-inline-images t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-agenda-files (expand-file-name "orgfiles" pew/home-dir)))

(provide 'pkg-org)
;;; pkg-org.el ends here
