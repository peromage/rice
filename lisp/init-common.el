;;; init-common.el --- Common library -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the Pew common library file.
;; It might be splitted into several files if it's needed in the future.
;; NOTE: This file should be loaded before any other packages.

;;; Code:
;;; Start eval-and-compile
(eval-and-compile
;;; Buffer definitions
  (defvar pew/special-buffer-alist
    '(;; VC
      (magit . "^ *[Mm]agit")
      (vc . "^ *\\*[Vv][Cc]-.*\\*$")
      (ediff . "^ *\\*[Ee]diff.*\\*$")
      ;; Interactive
      (shell . "^ *\\*.*\\b[Ss]hell\\*$")
      (terminal . "^ *\\*.*\\b[Tt]erm\\(inal\\)?\\*$")
      (scratch . "^ *\\*[Ss]cratch\\*$")
      ;; Org mode
      (org-src . "^ *\\*[Oo]rg [Ss]rc .*\\*$")
      (org-export . "^ *\\*[Oo]rg .* [Ee]xport\\*$")
      ;; Edit mode
      (edit-indirect . "^ *\\*edit-indirect .*\\*$")
      ;; Man page
      (man . "^ *\\*[Mm]an .*\\*$")
      ;; Message and output
      (help . "^ *\\*.*\\b[Hh]elp\\*$")
      (message . "^ *\\*.*\\b[Mm]essages?\\*$")
      (backtrace . "^ *\\*.*\\b[Bb]acktrace\\*$")
      (warning . "^ *\\*.*\\b[Ww]arnings?\\*$")
      (log . "^ *\\*.*\\b[Ll]og\\*$")
      (compilation . "^ *\\*.*\\b[Cc]ompilation\\*$")
      (output . "^ *\\*.*\\b[Oo]utput\\*$")
      (command . "^ *\\*.*\\b[Cc]ommands?\\*$")
      ;; Starred
      (starred . "^ *\\*.*\\*$"))
    "An alist of special buffer pattern regex.")

  (defmacro pew/special-buffer (name &optional concated)
    "Return the corresponding buffer pattern with given NAME.
NAME should be one of the keys from `pew/special-buffer-alist'.
If NAME is a list then the result will be a list of matching patterns instead.
If CONCATED is non-nil the result will be concatenated with '\\|'."
    (declare (indent 0))
    (let ((l:result nil)
          (l:match nil)
          (l:getter (lambda (x) (assoc x pew/special-buffer-alist)))
          (l:error (lambda (x) (error "No matching special buffer for %s" x))))
      (cond
       ;; Multiple output
       ((not (symbolp name))
        (dolist (l:name name)
          (if (setq l:match (funcall l:getter l:name))
              (push (cdr l:match) l:result)
            (funcall l:error l:name)))
        (setq l:result (reverse l:result))
        (if concated
            (mapconcat #'identity l:result "\\|")
          (cons 'list l:result)))
       ;; Single output
       ((setq l:match (funcall l:getter name))
        (setq l:result (cdr l:match)))
       (t (funcall l:error name))))))
;;; End eval-and-compile

;;; Debugging
(defun pew/reload-init-file ()
  "Reload the config file."
  (interactive)
  (load-file user-init-file))

(defun pew/open-init-file ()
  "Open the config file."
  (interactive)
  (find-file user-init-file))

(defmacro pew/expand-macro (form &optional all)
  "Expand the macro in FORM and print the expanded results.
Possible value for ALL:
  nil              - call `macroexpand'
  1                - call `macroexpand-1'
  any other values - call `macroexpand-all'
The result will be shown in message buffer.  Return nil to reduce confusion."
  (declare (indent 0))
  (let ((l:helper (lambda (fn fm) (message "%s: %S" fn (funcall fn fm)) nil)))
    (pcase all
      ('nil `(,l:helper 'macroexpand ',form))
      ('1 `(,l:helper 'macroexpand-1 ',form))
      (_ `(,l:helper 'macroexpand-all ',form)))))

(defun pew/keycode-to-string (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode to name: ")
  (let ((l:name (help-key-description (vector keycode) nil)))
    (message l:name)))

(defun pew/buffer-full-path ()
  "Display current file path in the minibuffer."
  (interactive)
  (message buffer-file-name))

;;; Paths
(defun pew/normalize-path (base &optional component follow)
  "Normalize path BASE by removing relative representations.
If BASE is a relative path the result will be a path which is relative to the
current path.
When COMPONENT is given it will be appended at the end of BASE.
When FOLLOW is non-nil the result will an absolute path with all symlink
resolved."
  (let ((l:result (expand-file-name (file-name-concat base component))))
    (if follow (file-truename l:result) l:result)))

;;; Editor
(defun pew/delete-trailing-whitespaces ()
  "Clear trailing whitespaces in current buffer."
  (delete-trailing-whitespace (point-min) (point-max)))

;;; Buffers
(defvar pew/hidden-buffers (pew/special-buffer (magit starred))
  "Buffers that are hiddens in general scenarios.")

(defun pew/hidden-buffer-p (name)
  "Check if the given buffer NAME is a hidden buffer.
Return t if NAME matches one of patterns defined in `pew/hidden-buffers' or nil
if there is not match."
  (let ((l:hiddens pew/hidden-buffers)
        (l:matched nil))
    (while (and (not l:matched) l:hiddens)
      (setq l:matched (string-match (pop l:hiddens) name)))
    l:matched))

(defun pew/switch-buffer (&optional prev)
  "Switch to the next buffer and skip hidden buffers.
If PREV is non-nil switch to the previous buffer.
Use `pew/hidden-buffer-p' to filter buffers."
  (let ((l:current-buffer (current-buffer))
        (l:switch-func (if prev #'previous-buffer #'next-buffer)))
    (funcall l:switch-func)
    (while (and (pew/hidden-buffer-p (buffer-name))
                (not (eq l:current-buffer (current-buffer))))
      (funcall l:switch-func))))

(defun pew/next-buffer ()
  "Switch to the next buffer but skip hidden buffers."
  (interactive)
  (pew/switch-buffer))

(defun pew/prev-buffer ()
  "Switch to the previous buffer but skip hidden buffers."
  (interactive)
  (pew/switch-buffer t))

(defun pew/close-other-buffers-in-major-mode (mode)
  "Close all other buffers in major MODE but this one."
  (interactive "SMajor mode: ")
  (let ((l:this-buffer (current-buffer)))
    (dolist (l:buffer (buffer-list))
      (if (and (eq mode (buffer-local-value 'major-mode l:buffer))
               (not (eq l:this-buffer l:buffer)))
          (kill-buffer l:buffer)))))

;;; Windows
(defun pew/side-window-p (window)
  "Return non-nil if WINDOW is a side window."
  (window-parameter window 'window-side))

(defun pew/side-window-exists-p (&optional side)
  "Return the first side window if there are otherwise nil.
If SIDE is given and is one of 'top' 'bottom' 'left' and 'right', check for that
specified side.  If SIDE is nil it means check all sides."
  (window-with-parameter 'window-side side))

(defun pew/window-list-side ()
  "Return a list of side windows."
  (seq-filter
   (lambda (x) (pew/side-window-p x))
   (window-list)))

(defun pew/window-list-normal ()
  "Return a list of normal (non-side) windows."
  (seq-filter
   (lambda (x) (not (pew/side-window-p x)))
   (window-list)))

(defun pew/pop-window-in-new-tab ()
  "Pop current window into a new tab.
Side window can also be poped."
  (interactive)
  (tab-bar-new-tab)
  (if (pew/side-window-p (selected-window))
      ;; Side window cannot be maximized so pick a normal window and switch to it
      (let ((l:current-buffer (current-buffer)))
        (select-window (car (pew/window-list-normal)))
        (switch-to-buffer l:current-buffer)))
  (delete-other-windows))

(defun pew/next-window ()
  "Switch to the next window."
  (interactive)
  (other-window 1))

(defun pew/prev-window ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(defun pew/close-window ()
  "Close window and the tab if there is only one window left."
  (interactive)
  ;; If there is only one normal window left and side windows exist, close the tab
  (if (and (not (pew/side-window-p (selected-window)))
           (equal 1 (length (pew/window-list-normal))))
      (tab-bar-close-tab)
    (delete-window)))

(defun pew/scroll-other-window-page-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window))

(defun pew/scroll-other-window-page-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window-down))

(defun pew/scroll-other-window-line-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window 1))

(defun pew/scroll-other-window-line-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window -1))

(defun pew/recenter-other-window ()
  "Scroll other window one page down."
  (interactive)
  (recenter-other-window))

;;; Tabs
(defun pew/move-tab-next ()
  "Move current tab to the next."
  (interactive)
  (tab-bar-move-tab 1))

(defun pew/move-tab-prev ()
  "Move current tab to the previous."
  (interactive)
  (tab-bar-move-tab -1))

;;; Themes
(defun pew/load-theme (theme)
  "Load THEME but make sure it is the only one active."
  (interactive (list '__PEW_LOAD_THEME__))
  (if (eq '__PEW_LOAD_THEME__ theme)
      (call-interactively #'load-theme)
    (load-theme theme t))
  ;; Disable the rest of the themes
  (if (> (length custom-enabled-themes) 1)
      (dolist (l:theme (cdr custom-enabled-themes))
        (disable-theme l:theme))))

;;; Frames
(defvar pew/frame-opacity-adjust-step 10
  "The amount of opacity changed each time.
Used by `pew/increase-frame-opacity'and `pew/decrease-frame-opacity'.")

(defun pew/set-frame-opacity (val)
  "Set the opacity of the current frame.
VAL is a number between 0 and 100.  0=transparent/100=opaque"
  (interactive "nFrame Opacity [transparent(0) - opaque(100)]: ")
  (let ((l:value (cond ((> val 100) 100)
                      ((< val 0) 0)
                      (t val))))
    (message "Set Frame opacity: %d%%" l:value)
    (set-frame-parameter (selected-frame) 'alpha (cons l:value l:value))))

(defun pew/increase-frame-opacity ()
  "Increase frame opacity."
  (interactive)
  (pew/set-frame-opacity (+ (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                            pew/frame-opacity-adjust-step)))

(defun pew/decrease-frame-opacity ()
  "Decrease frame opacity."
  (interactive)
  (pew/set-frame-opacity (- (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                            pew/frame-opacity-adjust-step)))

;;; Dired
(defun pew/dired-go-to ()
  "Go into the target under the cursor without creating a new buffer."
  (interactive)
  (dired-find-alternate-file))

(defun pew/dired-go-up ()
  "Go to the parent directory without creating a new buffer."
  (interactive)
  ;; Create a new buffer of the parent path
  (dired-up-directory)
  ;; Go back to the current path
  (dired-find-file)
  ;; Go up and close the current path buffer then the cursor will be on the current path
  (find-alternate-file ".."))

(defun pew/dired-close-others ()
  "Close other Dired buffers but this one."
  (interactive)
  (pew/close-other-buffers-in-major-mode 'dired-mode))

;;; Hook functions
(defun pew/terminal-mode-on-init ()
  "Common setup for terminal/shell modes."
  (setq-local word-wrap nil)
  (setq-local truncate-lines nil)
  (setq-local truncate-partial-width-windows nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local global-hl-line-mode nil)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun pew/text-mode-on-init ()
  "Common setup for text processing modes."
  (setq-local line-move-visual t)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (display-line-numbers-mode -1)
  (set-fill-column -1)
  (electric-pair-mode -1)
  (electric-indent-mode -1))

(defun pew/reuse-window-in-buffer ()
  "Make new spawned windows atttempt to reuse current ones.
This is usually useful in some major modes like `grep-mode'."
  (setq-local display-buffer-base-action
              '((display-buffer-reuse-window
                 display-buffer-use-some-window)))
  (setq-local display-buffer-alist nil))

;;; String functions
(defun pew/file-to-string (path)
  "Read the file content at PATH and return a string.
From: http://xahlee.info/emacs/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun pew/file-to-string-lines (path)
  "Read the file content at PATH and return a list of lines.
From: http://xahlee.info/emacs/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(provide 'init-common)

;;; Data functions
(defun pew/gethash (table &rest keys)
  "Access a hashtable TABLE recursively with a list of KEYS.
This functions is similar to `gethash' but it allows user to specify a list of
keys in one go.
Especially useful when accessing a JSON object."
  (if (= 1 (length keys))
      (gethash (car keys) table)
    (apply #'pew/gethash (gethash (car keys) table) (cdr keys))))

;;; init-common.el ends here
