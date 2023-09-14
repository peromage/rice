;;; init-common.el --- Common library -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the Pew common library file.
;; It might be splitted into several files if it's needed in the future.
;; NOTE: This file should be loaded before any other packages.

;;; Code:
;;; Start eval-and-compile
(eval-and-compile
;;; Buffer definitions
  (defvar pew::special-buffer-alist
    '(;; VC
      (magit . "^ *[Mm]agit")
      (vc . "^ *\\*[Vv][Cc]-.*\\*$")
      (ediff . "^ *\\*[Ee]diff.*\\*$")
      ;; Interactive
      (shell . "^ *\\*\\(\\w+\\s-?\\|\\w+\\)?[Ss]h\\(ell\\)?\\*$")
      (terminal . "^ *\\*\\(\\w+\\s-?\\|\\w+\\)?[Tt]erm\\(inal\\)?\\*$")
      (scratch . "^ *\\*[Ss]cratch\\*$")
      ;; Org mode
      (org-starred . "^ *\\*[Oo]rg ")
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
      ;; General
      (starred . "^ *\\*.*\\*")
      (non-starred . "^ *[^* ]"))
    "An alist of special buffer pattern regex.")

  (defvar pew::hidden-buffer-list '(magit starred)
    "Buffers that are hiddens for general purposes.")

  (defun pew::special-buffer (key &optional in-list)
    "Return the corresponding buffer pattern with given KEY.
Key is a symbol and should be one of the keys from `pew::special-buffer-alist'.
Key can also be a list of symbols and the returned value will be a string
concatenated with '\\|'.
If IN-LIST is non-nil the returned value will be a list."
    (declare (indent 0))
    (let ((l:keys (if (listp key) key (list key)))
          (l:func (lambda (k)
                    (let (l:it)
                      (unless (setq l:it (assq k pew::special-buffer-alist))
                        (error "Invalid key: %S" k))
                      (cdr l:it)))))
      (if in-list
          (mapcar l:func l:keys)
        (mapconcat l:func l:keys "\\|"))))

  (defun pew::special-buffer-match-p (key name)
    "Check if given buffer NAME matches buffers defined by KEY.
KEY is the same one with `pew::special-buffer'."
    (string-match-p (pew::special-buffer key) name))

  (defun pew::side-window-actions (side slot)
    "Return a list of pre-configured side window actions.
See `display-buffer' for property SIDE, SLOT."
    `((display-buffer-reuse-window display-buffer-in-side-window)
      (reusable-frames . t)
      (inhibit-switch-frame . t)
      (window-height . 0.25)
      (side . ,side)
      (slot . ,slot)))) ;;; End eval-and-compile

;;; Debugging
(defun pew::reload-init-file ()
  "Reload the config file."
  (interactive)
  (load-file user-init-file))

(defun pew::open-init-file ()
  "Open the config file."
  (interactive)
  (find-file user-init-file))

(defmacro pew::expand-macro (form &optional step noprint)
  "Expand the macro in FORM and print the expanded results.
Possible value for STEP:
  nil              - call `macroexpand'
  1                - call `macroexpand-1'
  any other values - call `macroexpand-all'
The result will be shown in the message buffer.
If NOPRINT is non-nil, the expanded list will be returned instead of printing
out in the message buffer."
  (declare (indent 0))
  (let ((l:result (funcall (intern (format "macroexpand%s"
                                           (pcase step
                                             ('nil "")
                                             (1 "-1")
                                             (_ "-all"))))
                           form)))
    (if noprint
        `(quote ,l:result)
      (message "--- Begin macro expansion ---\n%S\n--- End macro expansion ---" l:result)
      t)))

(defun pew::display-keycode (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode: ")
  (message (key-description (vector keycode))))

(defun pew::display-buffer-path ()
  "Display current file path in the minibuffer."
  (interactive)
  (message buffer-file-name))

(defun pew::display-mode-inheritance (mode)
  "Display current major mode inheritance in the minibuffer.
If prefix argument is given, a mode name can be manually typed in.
If MODE is any non-nill value other than '(4), that mode name will be used."
  (interactive "P")
  (let ((l:mode-to-check (pcase mode
                           ('nil major-mode)
                           ('(4) (read))
                           (_ mode))))
    (named-let find-parent ((major-mode l:mode-to-check)
                            (results (list l:mode-to-check)))
      (let ((parent-major-mode (get major-mode 'derived-mode-parent)))
        (if (not parent-major-mode)
            (message "Inheritance: [ %s ]" (mapconcat (lambda (m) (format "%S" m)) results " <= "))
          (find-parent parent-major-mode (cons parent-major-mode results)))))))

;;; Paths
(defun pew::normalize-path (base &optional component follow)
  "Normalize path BASE by removing relative representations.
If BASE is a relative path the result will be a path which is relative to the
current path.
When COMPONENT is given it will be appended at the end of BASE.
When FOLLOW is non-nil the result will an absolute path with all symlink
resolved."
  (let ((l:result (expand-file-name (file-name-concat base component))))
    (if follow (file-truename l:result) l:result)))

;;; Editor
(defun pew::delete-trailing-whitespaces ()
  "Clear trailing whitespaces in current buffer."
  (delete-trailing-whitespace (point-min) (point-max)))

;;; Buffers
(defun pew::dired-buffer-p (name)
  "Check if the given buffer NAME is a Dired buffer."
  (eq 'dired-mode (buffer-local-value 'major-mode (get-buffer name))))

(defun pew::next-editing-buffer (&optional backwards)
  "Switch to the next editing buffer.
If BACKWARDS is non-nil switch to the previous one."
  (interactive "P")
  (let ((l:current-buffer (current-buffer))
        (l:switch-func (if backwards #'previous-buffer #'next-buffer)))
    (funcall l:switch-func)
    (while (and (not (eq l:current-buffer (current-buffer)))
                (or (pew::special-buffer-match-p pew::hidden-buffer-list (buffer-name))
                    (pew::dired-buffer-p (buffer-name))))
      (funcall l:switch-func))))

(defun pew::previous-editing-buffer ()
  "Like `pew::next-editing-buffer' but does it backwards."
  (interactive)
  (pew::next-editing-buffer :previous))

(defun pew::close-other-buffers-in-major-mode (mode)
  "Close all other buffers in major MODE but this one."
  (interactive "SMajor mode: ")
  (let ((l:this-buffer (current-buffer)))
    (dolist (l:buffer (buffer-list))
      (if (and (eq mode (buffer-local-value 'major-mode l:buffer))
               (not (eq l:this-buffer l:buffer)))
          (kill-buffer l:buffer)))))

;;; Windows
(defun pew::side-window-p (window)
  "Return non-nil if WINDOW is a side window."
  (window-parameter window 'window-side))

(defun pew::side-window-exists-p (&optional side)
  "Return the first side window if there is any, otherwise nil.
If SIDE is given and is one of 'top' 'bottom' 'left' and 'right', check for that
specified side.  If SIDE is nil it means check all sides."
  (window-with-parameter 'window-side side))

(defun pew::normal-window-p (window)
  "Return t if WINDOW is a normal window."
  (not (pew::side-window-p window)))

(defun pew::last-normal-window-p (window)
  "Return t if WINDOW is the last normal window."
  (and (pew::normal-window-p window)
       (= 1 (length (pew::list-normal-windows)))))

(defun pew::list-side-windows ()
  "Return a list of side windows."
  (seq-filter
   (lambda (x) (pew::side-window-p x))
   (window-list)))

(defun pew::list-normal-windows ()
  "Return a list of normal (non-side) windows."
  (seq-filter
   (lambda (x) (pew::normal-window-p x))
   (window-list)))

(defun pew::pop-window-in-new-tab (arg)
  "Pop the current window into a new tab.
If prefix ARG is presented, pop the window without deleting it from the original
place."
  (interactive "P")
  (let ((l:current-buffer (current-buffer)))
    (if (and (null arg) (not (pew::last-normal-window-p (selected-window))))
        (delete-window))
    (tab-bar-new-tab) ;; Duplicate current layout
    (select-window (car (pew::list-normal-windows)))
    (switch-to-buffer l:current-buffer)
    (delete-other-windows)))

(defun pew::pop-window-in-new-tab-persist ()
  "Like `pew::pop-window-in-new-tab' but keep the original window."
  (interactive)
  (pew::pop-window-in-new-tab :persist))

(defun pew::next-window ()
  "Switch to the next window."
  (interactive)
  (other-window 1))

(defun pew::prev-window ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(defun pew::close-window ()
  "Close the current window, or the tab if it is the last normal window."
  (interactive)
  (if (pew::last-normal-window-p (selected-window))
      ;; If there is only one normal window left, close the tab, regardless even
      ;; side windows exist
      (tab-bar-close-tab)
    (delete-window)))

(defun pew::scroll-other-window-page-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window))

(defun pew::scroll-other-window-page-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window-down))

(defun pew::scroll-other-window-line-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window 1))

(defun pew::scroll-other-window-line-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window -1))

(defun pew::recenter-other-window ()
  "Scroll other window one page down."
  (interactive)
  (recenter-other-window))

;;; Tabs
(defun pew::move-tab-next ()
  "Move current tab to the next."
  (interactive)
  (tab-bar-move-tab 1))

(defun pew::move-tab-prev ()
  "Move current tab to the previous."
  (interactive)
  (tab-bar-move-tab -1))

;;; Themes
(defun pew::load-theme (theme)
  "Load THEME but make sure it is the only one active."
  (interactive (list '__PEW_LOAD_THEME__))
  (if (eq '__PEW_LOAD_THEME__ theme)
      (call-interactively #'load-theme)
    (load-theme theme t))
  ;; Disable the rest of the themes
  (if (> (length custom-enabled-themes) 1)
      (dolist (l:theme (cdr custom-enabled-themes))
        (disable-theme l:theme))))

(defun pew::find-font (&rest args)
  "Return a font object is it's found on the current system.
ARGS is the same as the ones defined in `font-spec'.
Return nil if no match."
  (find-font (apply 'font-spec args)))

;;; Frames
(defvar pew::frame-opacity-adjust-step 5
  "The amount of opacity changed each time.
Used by `pew::increase-frame-opacity'and `pew::decrease-frame-opacity'.")

(defun pew::set-frame-opacity (val)
  "Set the opacity of the current frame.
VAL is a number between 0 and 100.  0=transparent/100=opaque"
  (interactive "nFrame Opacity [transparent(0) - opaque(100)]: ")
  (let ((l:value (cond ((> val 100) 100)
                       ((< val 0) 0)
                       (t val))))
    (message "Set Frame opacity: %d%%" l:value)
    (set-frame-parameter (selected-frame) 'alpha (cons l:value l:value))))

(defun pew::increase-frame-opacity ()
  "Increase frame opacity."
  (interactive)
  (pew::set-frame-opacity (+ (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                             pew::frame-opacity-adjust-step)))

(defun pew::decrease-frame-opacity ()
  "Decrease frame opacity."
  (interactive)
  (pew::set-frame-opacity (- (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                             pew::frame-opacity-adjust-step)))

(defun pew::pop-window-in-new-frame (arg)
  "Pop the current window into a new frame.
If prefix ARG is presented, pop the window without deleting it from the original
place."
  (interactive "P")
  (let ((l:current-buffer (current-buffer)))
    (if (and (null arg) (not (pew::last-normal-window-p (selected-window))))
        (delete-window))
    (select-frame (make-frame-command))
    (switch-to-buffer l:current-buffer)
    (delete-other-windows)))

(defun pew::pop-window-in-new-frame-persist ()
  "Like `pew::pop-window-in-new-frame' but keep the original window."
  (interactive)
  (pew::pop-window-in-new-frame :persist))

;;; Dired
(defun pew::dired-go-to ()
  "Go into the target under the cursor without creating a new buffer."
  (interactive)
  (dired-find-alternate-file))

(defun pew::dired-go-up ()
  "Go to the parent directory without creating a new buffer."
  (interactive)
  ;; Create a new buffer of the parent path
  (dired-up-directory)
  ;; Go back to the current path
  (dired-find-file)
  ;; Go up and close the current path buffer then the cursor will be on the current path
  (find-alternate-file ".."))

(defun pew::dired-close-others ()
  "Close other Dired buffers but this one."
  (interactive)
  (pew::close-other-buffers-in-major-mode 'dired-mode))

;;; Eshell
(defun pew::eshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (if (eq 'eshell-mode major-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input))
    (error "Not an Eshell buffer")))

;;; Hook functions
(defun pew::terminal-mode-oninit ()
  "Common setup for terminal/shell modes."
  (setq-local word-wrap nil)
  (setq-local truncate-lines nil)
  (setq-local truncate-partial-width-windows nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local global-hl-line-mode nil)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun pew::text-mode-oninit ()
  "Common setup for text processing modes."
  (setq-local line-move-visual t)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (display-line-numbers-mode -1)
  (set-fill-column -1)
  (electric-pair-mode -1)
  (electric-indent-mode -1))

(defun pew::reuse-window-in-buffer ()
  "Make new spawned windows atttempt to reuse current ones.
This is usually useful in some major modes like `grep-mode'."
  (setq-local display-buffer-base-action
              '((display-buffer-reuse-window
                 display-buffer-use-some-window)))
  (setq-local display-buffer-alist nil))

;;; String functions
(defun pew::file-to-string (path)
  "Read the file content at PATH and return a string.
From: http://xahlee.info/emacs/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun pew::file-to-string-lines (path)
  "Read the file content at PATH and return a list of lines.
From: http://xahlee.info/emacs/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

;;; Number functions
(defun pew::evenp (num)
  "Determine if NUM is odd."
  (zerop (mod num 2)))

(defun pew::oddp (num)
  "Determine if NUM is odd."
  (not (pew::evenp num)))

;;; Data functions
(defun pew::gethash (table &rest keys)
  "Access a hashtable TABLE recursively with a list of KEYS.
This functions is similar to `gethash' but it allows user to specify a list of
keys in one go.
Especially useful when accessing a JSON object."
  (if (= 1 (length keys))
      (gethash (car keys) table)
    (apply #'pew::gethash (gethash (car keys) table) (cdr keys))))

(defmacro pew::swap (a b)
  "Swap values in A and B.
NOTE: A and B must be lvalues."
  `(setq ,a (prog1 ,b (setq ,b ,a))))

(defun pew::load-data-file (file)
  "Read the FILE and return a Lisp data object.
Only the first list will be read."
  (read (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))))

(defun pew::save-data-file (file obj)
  "Save a Lisp data OBJ to the FILE.
Existing content will be overwritten."
  (with-temp-file file
    (insert ";;; -*- coding: utf-8; mode: lisp-data; -*-\n")
    (pp obj (current-buffer))))

(provide 'init-common)
;;; init-common.el ends here
