;;; init-common.el --- Common library -*- lexical-binding: t -*-
;;; Commentary:
;; This is the PEW common library file.
;; It might be splitted into several files if it's needed in the future.
;; NOTE: This file should be loaded before any other packages.

;;; Code:
;;; Things which need to be evaluated at compile-time
(eval-and-compile
;;;; Buffer definitions
  (defvar pew/special-buffer-alist
    '((magit . "^ *[Mm]agit")
      (vc . "^ *\\*[Vv][Cc]-.*\\*$")
      (ediff . "^ *\\*[Ee]diff.*\\*$")
      (shell . "^ *\\*.*[Ss]hell\\*$")
      (term . "^ *\\*.*[Tt]erm\\(inal\\)?\\*$")
      (help . "^ *\\*.*[Hh]elp\\*$")
      (message . "^ *\\*.*[Mm]essages\\*$")
      (backtrace . "^ *\\*.*[Bb]acktrace\\*$")
      (warning . "^ *\\*.*[Ww]arnings\\*$")
      (log . "^ *\\*.*[Ll]og\\*$")
      (compilation . "^ *\\*.*[Cc]ompilation\\*$")
      (output . "^ *\\*.*[Oo]utput\\*$")
      (scratch . "^ *\\*[Ss]cratch\\*$")
      (org-src . "^ *\\*[Oo]rg [Ss]rc .*\\*$")
      ;; General special definitions go last
      (starred . "^ *\\*.*\\*$"))
    "An alist of special buffer pattern regex.")

  (defmacro pew/special-buffer (name &optional concated)
    "Return the corresponding buffer pattern with given NAME.
NAME should be one of the keys from `pew/special-buffer-alist'.
If NAME is a list then the result will be a list of matching patterns instead.
If CONCATED is non-nil the result will be concatenated with '\\|'."
    (declare (indent 0))
    (let ((Lresult nil)
          (Lmatch nil)
          (Lgetter (lambda (x) (assoc x pew/special-buffer-alist)))
          (Lerror (lambda (x) (error "No matching special buffer for %s" x))))
      (cond
       ;; Multiple output
       ((not (symbolp name))
        (dolist (Lname name)
          (if (setq Lmatch (funcall Lgetter Lname))
              (push (cdr Lmatch) Lresult)
            (funcall Lerror Lname)))
        (setq Lresult (reverse Lresult))
        (if concated
            (mapconcat #'identity Lresult "\\|")
          (cons 'list Lresult)))
       ;; Single output
       ((setq Lmatch (funcall Lgetter name))
        (setq Lresult (cdr Lmatch)))
       (t (funcall Lerror name)))))

;;;; Configuration helpers
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
    (let ((Lhelper nil)
          (Lsection nil)
          (Lresult '(progn)))
      (dolist (Litem args)
        (cond
         ;; Expand form with its helper
         ((not (symbolp Litem))
          (push (list Lhelper Litem) Lresult))
         ;; Check keyword
         ((setq Lsection (assoc Litem pew/config-keywords))
          (setq Lhelper (cdr Lsection)))
         (t (error "Wrong keyword: %s" Litem))))
      (reverse Lresult)))

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
    (let ((Lmap (intern (format "%s-map" (car form))))
          (Lbindings (cdr form)))
      `(let ((LLkeyMap (make-sparse-keymap)))
         (dolist (LLbind ',Lbindings)
           (define-key LLkeyMap (pew/tokey (car LLbind)) (cdr LLbind)))
         (defvar ,Lmap LLkeyMap "Created by `pew/set-map'"))))

  (defmacro pew/set-bind (form)
    "Bind keys with an existing map.
FORM is of the form:
  (MAP BINDINGS)
Where MAP implies suffix '-map' and BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'."
    (declare (indent 0))
    (let ((Lmap (intern (format "%s-map" (car form))))
          (Lbindings (cdr form)))
      `(dolist (LLbind ',Lbindings)
         (define-key ,Lmap (pew/tokey (car LLbind)) (cdr LLbind)))))

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
    (let* ((Lcmd (car form))
           (LcmdRepeat (intern (format "%s-repeat" Lcmd)))
           (LcmdDocString "Created by `pew/set-transient'"))
      `(let ((LLmap (pew/set-map ,form)))
         (defun ,Lcmd ()
           ,LcmdDocString
           (interactive)
           (message "%s activated" ',Lcmd)
           (set-transient-map (symbol-value LLmap) nil))
         (defun ,LcmdRepeat ()
           ,LcmdDocString
           (interactive)
           (message "%s activated" ',LcmdRepeat)
           (set-transient-map (symbol-value LLmap) t)))))

  (defmacro pew/set-switch (form)
    "Create a command to switch variable between values.
FORM is of the form:
  (VAR VAL)
Where VAL can be nil or a list.  If VAL is nil then VAR will be switched between
non-nil and nil each time the command is called, otherwise cycle values from the
list.
The created command will be 'switch/VAR'."
    (declare (indent 0))
    (let* ((Lvar (car form))
           (Lval (cadr form))
           (Lswitch (intern (format "switch/%s" Lvar))))
      (if (not Lval)
          ;; On-off switch
          `(defun ,Lswitch ()
             ,(format "Switch variable `%s' between non-nil and nil.
Created by `pew/set-switch'." Lvar)
             (interactive)
             (setq ,Lvar (not ,Lvar))
             (message "%s: %s" ',Lvar (if ,Lvar "enabled" "disabled")))
        ;; Rotate switch
        `(defun ,Lswitch ()
           ,(format "Switch variable `%s' in the following values
  %S
Created by `pew/set-switch'." Lvar Lval)
           (interactive)
           (let* ((LLlist ,Lval)
                  (LLmatch (pew/rotate-head LLlist ,Lvar 'next)))
             (if LLmatch (setq ,Lvar (car LLmatch))
               ;; Reset the variable if no match
               (setq ,Lvar (car LLlist)))
             (message "%s: %s" ',Lvar ,Lvar))))))

  (defmacro pew/set-face (form)
    "Set face attributes.
FORM is of the form:
  (FACE ARGS)
Where FACE is the name and ARGS comes in pairs ATTRIBUTE VALUE.
See `set-face-attribute'."
    (declare (indent 0))
    `(set-face-attribute ',(car form) nil ,@(cdr form)))

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
    (let ((Lhook (intern (format "%s-hook" (car form))))
          (Lfunc (cdr form)))
      `(add-hook ',Lhook #',Lfunc)))

  (defmacro pew/set-eval (form)
    "Simply evaluate FORM and nothing else."
    form)

;;;; Macro utilities
  (defmacro pew/swap (a b)
    "Swap values in A and B."
    `(setq ,a (prog1 ,b (setq ,b ,a))))

  (defmacro pew/tokey (key)
    "Convert KEY to the representation that can be recognized as a keycord.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
    `(let ((LLkey ,key)) (if (stringp LLkey) (kbd LLkey) LLkey)))

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
           `(let ((LLlist ,list)) (append (cdr LLlist) (cons (car LLlist) nil))))
          (t
           `(let ((LLlist ,list)) (append (last LLlist) (butlast LLlist))))))

  (defmacro pew/rotate-head (list value &optional next)
    "Rotate LIST and find the matching VALUE.
When NEXT is non-nil the returned list head will be the followed value of the
matching one (VALUE will be on the tail).
Return a new list with VALUE is the first element.  Or nil when either LIST is
nil or VALUE is not found."
    `(let* ((LLlist ,list)
            (LLcond LLlist)
            (LLvalue ,value)
            (LLtail nil))
       (while LLcond
         (if (equal LLvalue (car LLcond))
             (setq LLtail LLcond
                   LLcond nil)
           (pop LLcond)))
       (if (not LLtail) nil
         (setq LLtail (append LLtail (butlast LLlist (length LLtail))))
         ,(if next '(pew/rotate LLtail) 'LLtail)))))

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
  (let ((Lhelper (lambda (fn fm) (message "%s: %S" fn (funcall fn fm)) nil)))
    (pcase all
      ('nil `(,Lhelper 'macroexpand ',form))
      ('1 `(,Lhelper 'macroexpand-1 ',form))
      (_ `(,Lhelper 'macroexpand-all ',form)))))

(defun pew/keycode-to-string (keycode)
  "Display corresponding key name from KEYCODE."
  (interactive "nKeycode to name: ")
  (let ((Lname (help-key-description (vector keycode) nil)))
    (message Lname)))

(defun pew/buffer-full-path ()
  "Display current file path in the minibuffer."
  (interactive)
  (message buffer-file-name))

;;; Common functions and commands
(defun pew/normalize-path (base &optional component follow)
  "Normalize path BASE by removing relative representations.
If BASE is a relative path the result will be a path which is relative to the
current path.
When COMPONENT is given it will be appended at the end of BASE.
When FOLLOW is non-nil the result will an absolute path with all symlink
resolved."
  (let ((Lresult (expand-file-name (file-name-concat base component))))
    (if follow (file-truename Lresult) Lresult)))

(defvar pew/home-dir (pew/normalize-path load-file-name "../..")
  "The PEW configuration's home directory.
Not necessarily to be `user-emacs-directory' since this configuration can be
loaded from other places.")

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
  (let ((Lhiddens pew/hidden-buffers)
        (Lmatched nil))
    (while (and (not Lmatched) Lhiddens)
      (setq Lmatched (string-match (pop Lhiddens) name)))
    Lmatched))

(defun pew/switch-buffer (switch-func)
  "Switch to the buffer by SWITCH-FUNC but skip hidden buffers.
Use `pew/hidden-buffer-p' to filter buffers."
  (let ((LcurrentBufferName (buffer-name)))
    (funcall switch-func)
    (while (and (pew/hidden-buffer-p (buffer-name))
                (not (string= LcurrentBufferName (buffer-name))))
      (funcall switch-func))))

(defun pew/next-buffer ()
  "Switch to the next buffer but skip hidden buffers."
  (interactive)
  (pew/switch-buffer #'next-buffer))

(defun pew/prev-buffer ()
  "Switch to the previous buffer but skip hidden buffers."
  (interactive)
  (pew/switch-buffer #'previous-buffer))

(defun pew/close-other-buffers-in-major-mode (mode)
  "Close all other buffers in major MODE but this one."
  (interactive "SMajor mode: ")
  (let ((LthisBuffer (current-buffer)))
    (dolist (Lbuffer (buffer-list))
      (if (and (eq mode (buffer-local-value 'major-mode Lbuffer))
               (not (eq LthisBuffer Lbuffer)))
          (kill-buffer Lbuffer)))))

;;; Windows
(defun pew/pop-window-in-new-tab ()
  "Pop current window into a new tab."
  (interactive)
  (tab-bar-new-tab)
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
  (cond ((one-window-p)
         (tab-bar-close-tab)
         (previous-window))
        (t (delete-window))))

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
      (dolist (Ltheme (cdr custom-enabled-themes))
        (disable-theme Ltheme))))

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
(defun pew/term-setup ()
  "Common setup for terminal/shell modes."
  (setq-local word-wrap nil
              truncate-lines nil
              truncate-partial-width-windows nil
              show-trailing-whitespace nil
              global-hl-line-mode nil)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun pew/text-setup ()
  "Common setup for text processing modes."
  (setq-local line-move-visual t)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (display-line-numbers-mode -1)
  (electric-pair-mode -1)
  (electric-indent-mode -1))

(defun pew/reuse-window-setup ()
  "Hook for certains modes that keep spawning new windows e.g. `grep-mode'."
  (setq-local display-buffer-base-action '((display-buffer-reuse-window
                                            display-buffer-use-some-window))
              display-buffer-alist nil))

(provide 'init-common)
;;; init-common.el ends here
