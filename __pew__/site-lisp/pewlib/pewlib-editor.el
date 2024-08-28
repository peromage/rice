;;; pewlib-editor.el --- Editting functions -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; In buffer
(defun /ns/delete-trailing-whitespaces ()
  "Clear trailing whitespaces in current buffer."
  (delete-trailing-whitespace (point-min) (point-max)))

(defun /ns/indent-space-in-buffer ()
  "Use spaces for indentation in buffer."
  (setq-local indent-tabs-mode nil
              tab-width 4))

(defun /ns/as-terminal-mode ()
  "Common setup for terminal/shell modes."
  (setq-local word-wrap nil
              truncate-lines nil
              truncate-partial-width-windows nil
              show-trailing-whitespace nil)
  (hl-line-mode -1)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun /ns/as-text-mode ()
  "Common setup for text processing modes."
  (setq-local line-move-visual t)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (whitespace-mode 1)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun /ns/as-prog-mode ()
  "Common setup for programming modes."
  (electric-indent-local-mode 1)
  (electric-pair-local-mode 1)
  (electric-quote-local-mode -1)
  (electric-layout-local-mode -1)
  (outline-minor-mode 1)
  (whitespace-mode 1)
  (display-line-numbers-mode 1)
  (display-fill-column-indicator-mode 1))

(provide 'pewlib-editor)
;;; pewlib-editor.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("/ns/" . "pewlib::editor::"))
;; End:
