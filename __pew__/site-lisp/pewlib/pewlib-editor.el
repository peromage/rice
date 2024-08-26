;;; pewlib-editor.el --- Editting functions -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; In buffer
(defun pewlib::editor::delete-trailing-whitespaces ()
  "Clear trailing whitespaces in current buffer."
  (delete-trailing-whitespace (point-min) (point-max)))

(defun pewlib::editor::indent-space-in-buffer ()
  "Use spaces for indentation in buffer."
  (setq-local indent-tabs-mode nil
              tab-width 4))

(defun pewlib::editor::as-terminal-mode ()
  "Common setup for terminal/shell modes."
  (setq-local word-wrap nil
              truncate-lines nil
              truncate-partial-width-windows nil
              show-trailing-whitespace nil
              global-hl-line-mode nil)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun pewlib::editor::as-text-mode ()
  "Common setup for text processing modes."
  (setq-local line-move-visual t)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (display-line-numbers-mode -1)
  (set-fill-column -1))

(provide 'pewlib-editor)
;;; pewlib-editor.el ends here
