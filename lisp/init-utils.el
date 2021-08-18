;;; init-utils.el --- Utilities -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files. Templates was from purcell/emacs.d.

;;; Code:

;; Debug functions
(defun peco/reload-initel ()
  (interactive)
  (load-file user-init-file))

(defun peco/open-initel ()
  (interactive)
  (find-file user-init-file))

(provide 'init-utils)

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
