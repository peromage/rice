;;; test-startup.el --- Test startup with packages loaded -*- lexical-binding: t; -*-

;;; Commentary:
;; This is a ELisp script and it should be executed with "emacs --script".
;;
;; Basic sanity test for configuration startup.

;;; Code:
(require 'url-vars)
(let ((debug-on-error t)
      (url-show-status nil)
      (user-emacs-directory default-directory)
      (user-init-file (expand-file-name "init.el" default-directory))
      (load-path (delq default-directory load-path)))
  (load-file user-init-file)
  (run-hooks 'after-init-hook)
  (run-hooks 'emacs-startup-hook))

(provide 'test-startup)
;;; test-startup.el ends here
