;;; init-base.el --- Basic environment initialization -*- lexical-binding: t -*-
;;; Commentary:

;; This is the basic Emacs environment initialization e.g. GC, daemon and etc..

;;; Code:

;;------------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;------------------------------------------------------------------------------

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;------------------------------------------------------------------------------
;; Allow access from the emacsclient.
;;------------------------------------------------------------------------------

(defun pew/start--emacs-daemon ()
  "Start a daemon if it is not running yet."
  (require 'server)
  (unless (server-running-p)
    (message "[pew] Starting Emacs daemon...")
    (server-start)))

(add-hook 'after-init-hook #'pew/start--emacs-daemon)

;;------------------------------------------------------------------------------
;; Temporary local files
;;------------------------------------------------------------------------------

;; Cache directory for local files
(let ((local-cache-dir (expand-file-name "tempfiles" user-emacs-directory))
      (file-settings
       '((bookmark-default-file . "bookmarks")
         (recentf-save-file . "recentf")
         (nsm-settings-file . "network-security.data")
         (ido-save-directory-list-file . "ido.last")
         (lsp-session-file . ".lsp-session-v1")
         (projectile-cache-file . "projectile.cache")
         (projectile-known-projects-file . "projectile-bookmarks.eld"))))
  (unless (file-directory-p local-cache-dir)
    (make-directory local-cache-dir t))
  ;; Local files
  (dolist (settings file-settings)
    (eval `(setq ,(car settings) ,(expand-file-name (cdr settings) local-cache-dir)))))

(provide 'init-base)
;;; init-base.el ends here
