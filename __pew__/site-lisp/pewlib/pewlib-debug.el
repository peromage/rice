;;; pewlib-debug.el --- Debugging functions -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; Themes
(defun /ns/load-theme (theme)
  "Load THEME but make sure it is the only one active."
  (interactive (list '__PEW_LOAD_THEME__))
  (if (eq '__PEW_LOAD_THEME__ theme)
      (call-interactively #'load-theme)
    (load-theme theme t))
  ;; Disable the rest of the themes
  (if (> (length custom-enabled-themes) 1)
      (dolist (theme (cdr custom-enabled-themes))
        (disable-theme theme))))

(defun /ns/find-font (&rest args)
  "Return a font object is it's found on the current system.
ARGS is the same as the ones defined in `font-spec'.
Return nil if no match."
  (find-font (apply 'font-spec args)))

;;; Lisp data file
(defun /ns/load-data-file (file)
  "Read the FILE and return a Lisp data object.
Only the first list will be read."
  (read (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))))

(defun /ns/save-data-file (file obj)
  "Save a Lisp data OBJ to the FILE.
Existing content will be overwritten."
  (with-temp-file file
    (insert ";;; -*- coding: utf-8; mode: lisp-data; -*-\n")
    (pp obj (current-buffer))))

;;; Debugging
(defun /ns/reload-init-file ()
  "Reload the config file."
  (interactive)
  (load-file user-init-file))

(defun /ns/open-init-file ()
  "Open the config file."
  (interactive)
  (find-file user-init-file))

(defun /ns/display-keycode (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode: ")
  (message "%s" (key-description (vector keycode))))

(defun /ns/display-buffer-path ()
  "Display current file path in the minibuffer."
  (interactive)
  (message buffer-file-name))

(defun /ns/display-mode-inheritance (mode)
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

(defmacro /ns/expand-macro (form &optional step noprint)
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

(provide 'pewlib-debug)
;;; pewlib-debug.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("/ns/" . "pewlib::debug::"))
;; End:
