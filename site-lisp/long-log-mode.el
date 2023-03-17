;;; long-log-mode.el --- Long log mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This major mode is based on `so-long-mode' with some additional tweaks.

;;; Code:
(require 'so-long)

(define-derived-mode long-log-mode so-long-mode "LongLog"
  "A major mode used for large text files.  Based on `so-long-mode'.
Buffers with this mode enabled are read-only by default."
  :interactive t
  :after-hook
  ;; Override the settings by `so-long-mode'
  (progn
    (toggle-truncate-lines 1)
    (hl-line-mode 1))
  ;; `long-log-mode' settings start from here
  (add-to-list 'so-long-minor-modes 'line-number-mode)
  (read-only-mode 1))

(provide 'long-log-mode)
;;; long-log-mode.el ends here
