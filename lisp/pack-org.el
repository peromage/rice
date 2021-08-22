;;; pack-org.el --- Org mode tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :init
  (setq org-src-fontify-natively t
        org-startup-folded nil
        org-return-follows-link t
        org-startup-truncated nil
        org-startup-with-inline-images t)
  :config
  (org-indent-mode -1))

(provide 'pack-org)
;;; pack-org.el ends here
