;;; init-utils.el --- Utilities -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files.  Templates was from purcell/emacs.d.

;;; Code:

;; Debug functions
(defun pew/reload-initel ()
  "Reload the config file."
  (interactive)
  (load-file user-init-file))

(defun pew/open-initel ()
  "Open the config file."
  (interactive)
  (find-file user-init-file))

(defun pew/find-keyname (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode to name: ")
  (message "%s" (help-key-description (vector keycode) nil)))

(provide 'init-utils)
;;; init-utils.el ends here
