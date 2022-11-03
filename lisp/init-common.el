;;; init-common.el --- Common library -*- lexical-binding: t -*-

;;; Commentary:
;; This is the PEW common library file.
;; It might be splitted into several files if it's needed in the future.
;; NOTE: This file should be loaded before any other packages.

;;; Code:

;; Things which need to be evaluated at compile-time
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
      (org-src . "^ *\\*[Oo]rg [Ss]rc .*\\*$")
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
    (let ((l/result nil)
          (l/match nil)
          (l/getter (lambda (x) (assoc x pew/special-buffer-alist)))
          (l/error (lambda (x) (error "No matching special buffer for %s" x))))
      (cond
       ;; Multiple output
       ((not (symbolp name))
        (dolist (l/name name)
          (if (setq l/match (funcall l/getter l/name))
              (push (cdr l/match) l/result)
            (funcall l/error l/name)))
        (setq l/result (reverse l/result))
        (if concated
            (mapconcat #'identity l/result "\\|")
          (cons 'list l/result)))
       ;; Single output
       ((setq l/match (funcall l/getter name))
        (setq l/result (cdr l/match)))
       (t (funcall l/error name)))))

;;; Configuration helpers
  (defvar pew/config-keywords
    '((:custom . pew/set-custom)
      (:map . pew/set-map)
      (:bind . pew/set-bind)
      (:transient . pew/set-transient)
      (:switch . pew/set-switch)
      (:face . pew/set-face)
      (:property . pew/set-property)
      (:hook . pew/set-hook)
      (:eval . pew/set-eval))
    "An alist of keywords used in `pew/config' to specify sections.
The value of each element is the expansion helper of that section.")

  (defmacro pew/config (&rest args)
    "Configuration helper.
The available keywords are registered in `pew/config-keywords'.
ARGS is a list of forms.  See section helpers for the form definitions."
    (declare (indent 0))
    (if (not (symbolp (car args)))
        (error "Missing keyword"))
    (let ((l/helper nil)
          (l/section nil)
          (l/result '(progn)))
      (dolist (l/item args)
        (cond
         ;; Expand form with its helper
         ((not (symbolp l/item))
          (push (list l/helper l/item) l/result))
         ;; Check keyword
         ((setq l/section (assoc l/item pew/config-keywords))
          (setq l/helper (cdr l/section)))
         (t (error "Wrong keyword: %s" l/item))))
      (reverse l/result)))

  (defmacro pew/set-custom (form)
    "Set custom variables or regular variables.
FORM is of the form:
  (VAR VALUE [COMMENT])
Underlying implementation uses `customize-set-variable'."
    (declare (indent 0))
    `(customize-set-variable ',(nth 0 form) ,(nth 1 form) ,(nth 2 form)))

  (defmacro pew/set-map (form)
    "Create a new map and bind keys with it.
FORM is of the form:
  (MAP BINDINGS)
Where MAP implies suffix '-map' and BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'.
Note: Unlike `pew/set-bind' this macro creates a new map.  It will not be
effective if the map already exists."
    (declare (indent 0))
    (let ((l/map (intern (format "%s-map" (car form))))
          (l/bindings (cdr form)))
      `(let ((lm/key-map (make-sparse-keymap)))
         (dolist (lm/bind ',l/bindings)
           (define-key lm/key-map (pew/tokey (car lm/bind)) (cdr lm/bind)))
         (defvar ,l/map lm/key-map "Created by `pew/set-map'"))))

  (defmacro pew/set-bind (form)
    "Bind keys with an existing map.
FORM is of the form:
  (MAP BINDINGS)
Where MAP implies suffix '-map' and BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'."
    (declare (indent 0))
    (let ((l/map (intern (format "%s-map" (car form))))
          (l/bindings (cdr form)))
      `(dolist (lm/bind ',l/bindings)
         (define-key ,l/map (pew/tokey (car lm/bind)) (cdr lm/bind)))))

  (defmacro pew/set-transient (form)
    "Create a command that enters transient mode when invoked.
FORM is of the form:
  (CMD BINDINGS)
Where CMD is the name of the command and BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'.
A map CMD-map and two commands CMD and CMD-repeat will be created.
Once CMD is invoked CMD-map will be temporarily activated.  The difference
between CMD and CMD-repeat is CMD only receive one followed key press while
CMD-repeat keeps receiving key press until an undefined key passed.
See `set-transient-map'.
Note: Obsoleted `repeat-map' property method in Emacs 28 since it didn't work
well for some reason:
  (put cmd 'repeat-map map-symbol)"
    (declare (indent 0))
    (let* ((l/cmd (car form))
           (l/cmd-repeat (intern (format "%s-repeat" l/cmd)))
           (l/cmd-doc-string "Created by `pew/set-transient'"))
      `(let ((lm/map (symbol-value (pew/set-map ,form))))
         (defun ,l/cmd ()
           ,l/cmd-doc-string
           (interactive)
           (message "%s activated" ',l/cmd)
           (set-transient-map lm/map nil))
         (defun ,l/cmd-repeat ()
           ,l/cmd-doc-string
           (interactive)
           (message "%s activated" ',l/cmd-repeat)
           (set-transient-map lm/map t)))))

  (defmacro pew/set-switch (form)
    "Create a command to switch variable between values.
FORM is of the form:
  (VAR VAL)
Where VAL can be nil or a list.  If VAL is nil then VAR will be switched between
non-nil and nil each time the command is called, otherwise cycle values from the
list.
The created command will be 'switch/VAR'."
    (declare (indent 0))
    (let* ((l/var (car form))
           (l/val (cadr form))
           (l/switch (intern (format "switch/%s" l/var))))
      (if (not l/val)
          ;; On-off switch
          `(defun ,l/switch ()
             ,(format "Switch variable `%s' between non-nil and nil.
Created by `pew/set-switch'." l/var)
             (interactive)
             (setq ,l/var (not ,l/var))
             (message "%s: %s" ',l/var (if ,l/var "enabled" "disabled")))
        ;; Rotate switch
        `(defun ,l/switch ()
           ,(format "Switch variable `%s' in the following values
  %S
Created by `pew/set-switch'." l/var l/val)
           (interactive)
           (let* ((lm/list ,l/val)
                  (lm/match (pew/rotate-head lm/list ,l/var 'next)))
             (if lm/match (setq ,l/var (car lm/match))
               ;; Reset the variable if no match
               (setq ,l/var (car lm/list)))
             (message "%s: %s" ',l/var ,l/var))))))

  (defmacro pew/set-face (form)
    "Set face attributes.
FORM is of the form:
  (FACE ARGS)
Where FACE is the name and ARGS comes in pairs ATTRIBUTE VALUE.
See `set-face-attribute'."
    (declare (indent 0))
    (let ((l/face (car form))
          (l/args (cdr form))
          (l/props nil))
      (while l/args
        (push (pop l/args) l/props)
        ;; Quote symbols
        (push (if (symbolp (car l/args))
                  (list 'quote (pop l/args))
                (pop l/args))
              l/props))
      `(set-face-attribute ',l/face nil ,@(reverse l/props))))

  (defmacro pew/set-property (form)
    "Set symbol's property.
FORM is of the form:
  (SYM PROP VAL)"
    (declare (indent 0))
    `(put ',(nth 0 form) ',(nth 1 form) ,(nth 2 form)))

  (defmacro pew/set-hook (form)
    "Set function to a hook.
FORM is a cons:
  (HOOK . FUNC)
Where HOOK implies suffix '-hook'."
    (declare (indent 0))
    (let ((l/hook (intern (format "%s-hook" (car form))))
          (l/func (cdr form)))
      `(add-hook ',l/hook #',l/func)))

  (defmacro pew/set-eval (form)
    "Simply evaluate FORM and nothing else."
    form)

;;; Macro utilities
  (defmacro pew/swap (a b)
    "Swap values in A and B."
    `(setq ,a (prog1 ,b (setq ,b ,a))))

  (defmacro pew/tokey (key)
    "Convert KEY to the representation that can be recognized as a keycord.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
    `(let ((lm/key ,key)) (if (stringp lm/key) (kbd lm/key) lm/key)))

  (defmacro pew/evenp (num)
    "Determine if NUM is odd."
    `(zerop (mod ,num 2)))

  (defmacro pew/oddp (num)
    "Determine if NUM is odd."
    `(not (zerop (mod ,num 2))))

  (defmacro pew/rotate (list &optional reverse)
    "Rotate the LIST by putting the first element to the last.
If REVERSE is non-nil the do it in a opposite way by putting the last element
to the first.
Return a new list or nil if LIST is nil."
    (cond ((not list) nil)
          ((not reverse)
           `(let ((lm/list ,list)) (append (cdr lm/list) (cons (car lm/list) nil))))
          (t
           `(let ((lm/list ,list)) (append (last lm/list) (butlast lm/list))))))

  (defmacro pew/rotate-head (list value &optional next)
    "Rotate LIST and find the matching VALUE.
When NEXT is non-nil the returned list head will be the followed value of the
matching one (VALUE will be on the tail).
Return a new list with VALUE is the first element.  Or nil when either LIST is
nil or VALUE is not found."
    `(let* ((lm/list ,list)
            (lm/cond lm/list)
            (lm/value ,value)
            (lm/tail nil))
       (while lm/cond
         (if (equal lm/value (car lm/cond))
             (setq lm/tail lm/cond
                   lm/cond nil)
           (pop lm/cond)))
       (if (not lm/tail) nil
         (setq lm/tail (append lm/tail (butlast lm/list (length lm/tail))))
         ,(if next '(pew/rotate lm/tail) 'lm/tail))))

  (defmacro pew/font (&rest args)
    "Return a font object is it's found on the current system.
ARGS is the same as the ones defined in `fond-spec'.
Return nil if no match."
    `(find-font (font-spec ,@args))))
;; Compile done

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
  (let ((l/helper (lambda (fn fm) (message "%s: %S" fn (funcall fn fm)) nil)))
    (pcase all
      ('nil `(,l/helper 'macroexpand ',form))
      ('1 `(,l/helper 'macroexpand-1 ',form))
      (_ `(,l/helper 'macroexpand-all ',form)))))

(defun pew/keycode-to-string (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode to name: ")
  (let ((l/name (help-key-description (vector keycode) nil)))
    (message l/name)))

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
  (let ((l/result (expand-file-name (file-name-concat base component))))
    (if follow (file-truename l/result) l/result)))

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
  (let ((l/hiddens pew/hidden-buffers)
        (l/matched nil))
    (while (and (not l/matched) l/hiddens)
      (setq l/matched (string-match (pop l/hiddens) name)))
    l/matched))

(defun pew/switch-buffer (&optional prev)
  "Switch to the next buffer and skip hidden buffers.
If PREV is non-nil switch to the previous buffer.
Use `pew/hidden-buffer-p' to filter buffers."
  (let ((l/current-buffer (current-buffer))
        (l/switch-func (if prev #'previous-buffer #'next-buffer)))
    (funcall l/switch-func)
    (while (and (pew/hidden-buffer-p (buffer-name))
                (not (eq l/current-buffer (current-buffer))))
      (funcall l/switch-func))))

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
  (let ((l/this-buffer (current-buffer)))
    (dolist (l/buffer (buffer-list))
      (if (and (eq mode (buffer-local-value 'major-mode l/buffer))
               (not (eq l/this-buffer l/buffer)))
          (kill-buffer l/buffer)))))

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
      (let ((l/current-buffer (current-buffer)))
        (select-window (car (pew/window-list-normal)))
        (switch-to-buffer l/current-buffer)))
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
      (dolist (l/theme (cdr custom-enabled-themes))
        (disable-theme l/theme))))

;;; Frames
(defun pew/set-frame-opacity (val)
  "Set the opacity of the current frame.
VAL is a number between 0 and 100.  0=transparent/100=opaque"
  (interactive "nFrame Opacity [transparent(0) - opaque(100)]: ")
  (let ((l/value (cond ((> val 100) 100)
                      ((< val 0) 0)
                      (t val))))
    (message "Frame opacity: %d" l/value)
    (set-frame-parameter (selected-frame) 'alpha (cons l/value l/value))))

(defvar pew/frame-opacity-change-step 10
  "The amount of opacity changed each time.
Used by `pew/increase-frame-opacity'and `pew/decrease-frame-opacity'.")

(defun pew/increase-frame-opacity ()
  "Increase frame opacity."
  (interactive)
  (let ((l/value (frame-parameter (selected-frame) 'alpha)))
    (if (consp l/value) (setq l/value (car l/value)))
    (pew/set-frame-opacity (+ l/value pew/frame-opacity-change-step))))

(defun pew/decrease-frame-opacity ()
  "Decrease frame opacity."
  (interactive)
  (let ((l/value (frame-parameter (selected-frame) 'alpha)))
    (if (consp l/value) (setq l/value (car l/value)))
    (pew/set-frame-opacity (- l/value pew/frame-opacity-change-step))))

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
(defun pew/terminal-common-setup ()
  "Common setup for terminal/shell modes."
  (setq-local word-wrap nil)
  (setq-local truncate-lines nil)
  (setq-local truncate-partial-width-windows nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local global-hl-line-mode nil)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun pew/text-common-setup ()
  "Common setup for text processing modes."
  (setq-local line-move-visual t)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (display-line-numbers-mode -1)
  (set-fill-column -1)
  (electric-pair-mode -1)
  (electric-indent-mode -1))

(defun pew/reuse-window-setup ()
  "Hook for certains modes that keep spawning new windows e.g. `grep-mode'."
  (setq-local display-buffer-base-action
              '((display-buffer-reuse-window
                 display-buffer-use-some-window)))
  (setq-local display-buffer-alist nil))

(provide 'init-common)
;;; init-common.el ends here
