;;; elpa-extra.el --- Extra interesting packages -*- lexical-binding: t -*-

;;; Commentary:
;; This collection is used to keep some interesting packages which however may
;; not be used in every-day work.

;;; Code:

(use-package?
  ;; Rainbow cat buffer progress bar
  nyan-mode
  ;; Colorful parenthesises
  rainbow-delimiters
  ;; Colorize color code
  rainbow-mode
  ;; Highlight current line
  beacon
  ;; Simplify S-expression editing
  paredit)

(provide 'elpa-extra)
;;; elpa-extra.el ends here
