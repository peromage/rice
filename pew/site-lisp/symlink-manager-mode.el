;;; symlink-manager-mode.el --- Symlink Manager Mode -*- lexical-binding: t; -*-

;;; Commentary:
;; The major mode that creates/deletes symlinks defined in the current buffer
;; content.

;;; Code:
(defun sm-create-link (target ask)
  "Create a link of TARGET to the current directory.
If ASK is a non-nil the link will be force overridden or prompted for
confirmation if it is an integer."
  (if (not (file-exists-p target)) nil
     (make-symbolic-link target default-directory ask)
     t))

(defun sm-delete-link (target)
  "Delete a link of TARGET from the current directory."
  (let ((f (file-name-nondirectory target)))
    (if (or (not (file-exists-p f)) (not (file-symlink-p f))) nil
      (delete-file f)
      t)))

(defun sm-read-line ()
  "Read the texts from the line where the cursor is."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun sm-move-next-line ()
  "Move cursor to the beginning of the next line."
  (interactive)
  (forward-line 1)
  (move-beginning-of-line 1))

(defun sm-move-previous-line ()
  "Move cursor to the beginning of the previous line."
  (interactive)
  (forward-line -1)
  (move-beginning-of-line 1))

(defun sm-do-create-link ()
  "Interactively create a symlink."
  (interactive)
  (let ((target (sm-read-line)))
    (if (sm-create-link target 1)
        (message "Linked target: %s" target)
      (user-error "Target not exists: %s" target))))

(defun sm-do-delete-link ()
  "Interactively delete a symlink."
  (interactive)
  (let ((target (sm-read-line)))
    (if (sm-delete-link target)
        (message "Unlinked target: %s" target)
      (user-error "Nothing to unlink for target: %s" target))))

(defvar sm-mode-map (let ((map (make-sparse-keymap)))
                      (dolist (binding '(("n" . sm-move-next-line)
                                         ("p" . sm-move-previous-line)
                                         ("s" . sm-do-create-link)
                                         ("d" . sm-do-delete-link)))
                        (define-key map (kbd (car binding)) (cdr binding)))
                      map))

(define-derived-mode sm-mode special-mode "Symgr"
  "Manage links defined in the buffer.
The content of the buffer should be a list of files/directories that will be
linked to.  The paths can be either relative to the content file or absolute.
Usually, the list is generated by find command.

For example, the content:

/somewhere/dir/file1
../../place/file2
...

The symbolic links will be created at the same directory of the content file."
  :interactive t)

(provide 'symlink-manager-mode)
;;; symlink-manager-mode.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("sm-" . "symlink-manager-"))
;; End:
