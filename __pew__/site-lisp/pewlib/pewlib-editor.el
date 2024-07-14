;;; pewlib-editor.el --- Editting functions -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; Editor
(defun pewlib::delete-trailing-whitespaces ()
  "Clear trailing whitespaces in current buffer."
  (delete-trailing-whitespace (point-min) (point-max)))

;;; Themes
(defun pewlib::load-theme (theme)
  "Load THEME but make sure it is the only one active."
  (interactive (list '__PEW_LOAD_THEME__))
  (if (eq '__PEW_LOAD_THEME__ theme)
      (call-interactively #'load-theme)
    (load-theme theme t))
  ;; Disable the rest of the themes
  (if (> (length custom-enabled-themes) 1)
      (dolist (theme (cdr custom-enabled-themes))
        (disable-theme theme))))

(defun pewlib::find-font (&rest args)
  "Return a font object is it's found on the current system.
ARGS is the same as the ones defined in `font-spec'.
Return nil if no match."
  (find-font (apply 'font-spec args)))

;;; Lisp data file
(defun pewlib::load-data-file (file)
  "Read the FILE and return a Lisp data object.
Only the first list will be read."
  (read (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))))

(defun pewlib::save-data-file (file obj)
  "Save a Lisp data OBJ to the FILE.
Existing content will be overwritten."
  (with-temp-file file
    (insert ";;; -*- coding: utf-8; mode: lisp-data; -*-\n")
    (pp obj (current-buffer))))

;;; Hook functions
(defun pewlib::terminal-mode-oninit ()
  "Common setup for terminal/shell modes."
  (setq-local word-wrap nil
              truncate-lines nil
              truncate-partial-width-windows nil
              show-trailing-whitespace nil
              global-hl-line-mode nil)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun pewlib::text-mode-oninit ()
  "Common setup for text processing modes."
  (setq-local line-move-visual t)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (display-line-numbers-mode -1)
  (set-fill-column -1))

(defun pewlib::reuse-window-in-buffer ()
  "Make new spawned windows atttempt to reuse current ones.
This is usually useful in some major modes like `grep-mode'."
  (setq-local display-buffer-base-action '((display-buffer-reuse-window
                                            display-buffer-use-some-window))
              display-buffer-alist nil))

;;; Debugging
(defun pewlib::reload-init-file ()
  "Reload the config file."
  (interactive)
  (load-file user-init-file))

(defun pewlib::open-init-file ()
  "Open the config file."
  (interactive)
  (find-file user-init-file))

(defmacro pewlib::expand-macro (form &optional step noprint)
  "Expand the macro in FORM and print the expanded results.
Possible value for STEP:
  nil              - call `macroexpand'
  1                - call `macroexpand-1'
  any other values - call `macroexpand-all'
The result will be shown in the message buffer.
If NOPRINT is non-nil, the expanded list will be returned instead of printing
out in the message buffer."
  (declare (indent 0))
  (let ((result (funcall (intern (format "macroexpand%s"
                                         (pcase step
                                           ('nil "")
                                           (1 "-1")
                                           (_ "-all"))))
                         form)))
    (if noprint
        `(quote ,result)
      (message "--- Begin macro expansion ---\n%s\n--- End macro expansion ---" (pp-to-string result))
      t)))

(defun pewlib::display-keycode (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode: ")
  (message "%s" (key-description (vector keycode))))

(defun pewlib::display-buffer-path ()
  "Display current file path in the minibuffer."
  (interactive)
  (message buffer-file-name))

(defun pewlib::display-mode-inheritance (mode)
  "Display current major mode inheritance in the minibuffer.
If prefix argument is given, a mode name can be manually typed in.
If MODE is any non-nill value other than '(4), that mode name will be used."
  (interactive "P")
  (let ((mode-to-check (pcase mode
                         ('nil major-mode)
                         ('(4) (read))
                         (_ mode))))
    (named-let find-parent ((major-mode mode-to-check)
                            (results (list mode-to-check)))
      (let ((parent-major-mode (get major-mode 'derived-mode-parent)))
        (if (not parent-major-mode)
            (message "Inheritance: [ %s ]" (mapconcat (lambda (m) (format "%S" m)) results " <= "))
          (find-parent parent-major-mode (cons parent-major-mode results)))))))

(provide 'pewlib-editor)
;;; pewlib-editor.el ends here
