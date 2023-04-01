;;; init-pewconfig.el --- Pew configurator -*- lexical-binding: t; -*-

;;; Commentary:
;; Code for Pew configurator.

;;; Code:
(require 'subr-x)
;;; Start eval-and-compile
(eval-and-compile
;;; The list of keywords
  (defvar pewconfig/keywords
    '((:custom . pewconfig/set-custom)
      (:map . pewconfig/set-map)
      (:bind . pewconfig/set-bind)
      (:transient . pewconfig/set-transient)
      (:switch . pewconfig/set-switch)
      (:face . pewconfig/set-face)
      (:property . pewconfig/set-property)
      (:hook . pewconfig/set-hook)
      (:automode . pewconfig/set-automode)
      (:eval . pewconfig/set-eval))
    "An alist of keywords used in `pewconfig' to specify sections.
The value of each element is the expansion helper of that section.")

;;; Main entry
  (defmacro pewconfig (&rest args)
    "Pew configuration utility.
This one is intended to be used for configuration entry instead of calling the
helper macros directly.
ARGS is a list of forms. See the registered helpers from `pewconfig/keywords'
for form definitions.
Typical usage is as follow:
  (pewconfig
    :custom
    (inhibit-startup-buffer-menu t)
    :bind
    (global
      (\"C-x C-d\" . dired-jump))
    ...) "
    (declare (indent 0))
    (named-let pewconfig-inner ((keyword-cons nil)
                                (running-list '(progn))
                                (args args))
      (if (not args)
          ;; Ending
          (reverse running-list)
        ;; Otherwise keep processing the elements
        (let ((l:head (assq (car args) pewconfig/keywords)))
          (if l:head
              ;; Look up for the next element if the head is a registered cons
              (pewconfig-inner l:head running-list (cdr args))
            ;; Otherwise update the list and move to the next element
            (pewconfig-inner keyword-cons (cons (list (cdr keyword-cons) (car args)) running-list) (cdr args)))))))

;;; :custom
  (defmacro pewconfig/set-custom (form)
    "Set custom variables or regular variables.
FORM is of the form:
  (VAR VALUE [COMMENT])
Underlying implementation uses `customize-set-variable'."
    (declare (indent 0))
    `(customize-set-variable ',(nth 0 form) ,(nth 1 form) ,(nth 2 form)))

;;; :map
  (defmacro pewconfig/set-map (form)
    "Create a new map and bind keys with it.
FORM is of the form:
  (MAP BINDINGS)
Where MAP implies suffix '-map' and BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'.
NOTE: Unlike `pewconfig/set-bind' this macro creates a new map.  It will not be
effective if the map already exists."
    (declare (indent 0))
    (let ((l:map (intern (format "%s-map" (car form))))
          (l:bindings (cdr form)))
      `(let ((ql:key-map (make-sparse-keymap)))
         (dolist (ql:bind ',l:bindings)
           (define-key ql:key-map (pew/tokey (car ql:bind)) (cdr ql:bind)))
         (defvar ,l:map ql:key-map "Created by `pewconfig/set-map'"))))

;;; :bind
  (defmacro pewconfig/set-bind (form)
    "Bind keys with an existing map.
FORM is of the form:
  (MAP BINDINGS)
Where MAP implies suffix '-map' and BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'."
    (declare (indent 0))
    (let ((l:map (intern (format "%s-map" (car form))))
          (l:bindings (cdr form)))
      `(dolist (ql:bind ',l:bindings)
         (define-key ,l:map (pew/tokey (car ql:bind)) (cdr ql:bind)))))

;;; :transient
  (defmacro pewconfig/set-transient (form)
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
NOTE: Discouraged `repeat-map' property method in Emacs 28 since it didn't work
well for some reason.  If `repeat-map' needs to be enabled, do:
  (put cmd 'repeat-map map-symbol)"
    (declare (indent 0))
    (let* ((l:cmd (car form))
           (l:cmd-repeat (intern (format "%s-repeat" l:cmd)))
           (l:cmd-doc-string "Created by `pewconfig/set-transient'"))
      `(let ((ql:map (symbol-value (pewconfig/set-map ,form))))
         (defun ,l:cmd (arg)
           "Temporarily activate a transient map.
Normally, when a key binding is invoked in the transient map, it will end and
return the previous key map.
ARG is a prefix argument.  If it is given, the transient map will keep activated
until `C-g' is pressed.
Created by `pewconfig/set-transient'."
           (interactive "P")
           (cond (arg
                  (message "%s activated in repeat mode" ',l:cmd)
                  (set-transient-map ql:map t))
                 (t
                  (message "%s activated" ',l:cmd)
                  (set-transient-map ql:map nil))))
         (defun ,l:cmd-repeat ()
           "Temporarily activate a transient map in repeat mode.
Created by `pewconfig/set-transient'."
           (interactive)
           (,l:cmd :repeat)))))

;;; :switch
  (defmacro pewconfig/set-switch (form)
    "Create a command to switch variable between values.
FORM is of the form:
  (VAR VAL)
Where VAL can be nil or a list.  If VAL is nil then VAR will be switched between
non-nil and nil each time the command is called, otherwise cycle values from the
list.
The created command will be 'switch/VAR'."
    (declare (indent 0))
    (let* ((l:var (car form))
           (l:val (cadr form))
           (l:switch (intern (format "switch/%s" l:var))))
      (if (not l:val)
          ;; On-off switch
          `(defun ,l:switch ()
             ,(format "Switch variable `%s' between non-nil and nil.
Created by `pewconfig/set-switch'." l:var)
             (interactive)
             (setq ,l:var (not ,l:var))
             (message "%s: %s" ',l:var (if ,l:var "enabled" "disabled")))
        ;; Rotate switch
        `(defun ,l:switch ()
           ,(format "Switch variable `%s' in the following values
  %S
Created by `pewconfig/set-switch'." l:var l:val)
           (interactive)
           (let* ((ql:list ,l:val)
                  (ql:match (pew/rotate-head ql:list ,l:var 'next)))
             (if ql:match (setq ,l:var (car ql:match))
               ;; Reset the variable if no match
               (setq ,l:var (car ql:list)))
             (message "%s: %s" ',l:var ,l:var))))))

;;; :face
  (defmacro pewconfig/set-face (form)
    "Set face attributes.
FORM is of the form:
  (FACE ARGS)
Where FACE is the name and ARGS comes in pairs ATTRIBUTE VALUE.
See `set-face-attribute'."
    (declare (indent 0))
    (let ((l:face (car form))
          (l:args (cdr form))
          (l:props nil))
      (while l:args
        (push (pop l:args) l:props)
        ;; Quote symbols
        (push (if (symbolp (car l:args))
                  (list 'quote (pop l:args))
                (pop l:args))
              l:props))
      `(set-face-attribute ',l:face nil ,@(reverse l:props))))

;;; :property
  (defmacro pewconfig/set-property (form)
    "Set symbol's property.
FORM is of the form:
  (SYM PROP VAL)"
    (declare (indent 0))
    `(put ',(nth 0 form) ',(nth 1 form) ,(nth 2 form)))

;;; :hook
  (defmacro pewconfig/set-hook (form)
    "Set function to a hook.
FORM is a cons:
  (HOOK . FUNC)
Where HOOK implies suffix '-hook'."
    (declare (indent 0))
    (let ((l:hook (intern (format "%s-hook" (car form))))
          (l:func (cdr form)))
      `(add-hook ',l:hook #',l:func)))

;;; :automode
  (defmacro pewconfig/set-automode (form)
    "Set `auto-mode-alist'.
FORM is a cons:
  (MATCHER . MODE)
Where MATCHER is usually a string of regex."
    (declare (indent 0))
    `(add-to-list 'auto-mode-alist ',form))

;;; :eval
  (defmacro pewconfig/set-eval (form)
    "Simply evaluate FORM and nothing else."
    form)

;;; Utility macros/functions
  (defmacro pew/swap (a b)
    "Swap values in A and B.
NOTE: A and B must be lvalues."
    `(setq ,a (prog1 ,b (setq ,b ,a))))

  (defun pew/tokey (key)
    "Convert KEY to the representation that can be recognized as a keycord.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
    (if (stringp key) (kbd key) key))

  (defun pew/evenp (num)
    "Determine if NUM is odd."
    (zerop (mod num 2)))

  (defun pew/oddp (num)
    "Determine if NUM is odd."
    (not (pew/evenp num)))

  (defmacro pew/rotate (list &optional reverse)
    "Rotate the LIST by putting the first element to the last.
If REVERSE is non-nil the do it in a opposite way by putting the last element
to the first.
Return a new list or nil if LIST is nil."
    (cond ((not list) nil)
          ((not reverse)
           `(let ((ql:list ,list)) (append (cdr ql:list) (cons (car ql:list) nil))))
          (t
           `(let ((ql:list ,list)) (append (last ql:list) (butlast ql:list))))))

  (defmacro pew/rotate-head (list value &optional next)
    "Rotate LIST and find the matching VALUE.
When NEXT is non-nil the returned list head will be the followed value of the
matching one (VALUE will be on the tail).
Return a new list with VALUE is the first element.  Or nil when either LIST is
nil or VALUE is not found."
    `(let* ((ql:list ,list)
            (ql:cond ql:list)
            (ql:value ,value)
            (ql:tail nil))
       (while ql:cond
         (if (equal ql:value (car ql:cond))
             (setq ql:tail ql:cond
                   ql:cond nil)
           (pop ql:cond)))
       (if (not ql:tail) nil
         (setq ql:tail (append ql:tail (butlast ql:list (length ql:tail))))
         ,(if next '(pew/rotate ql:tail) 'ql:tail))))

  (defun pew/find-font (&rest args)
    "Return a font object is it's found on the current system.
ARGS is the same as the ones defined in `font-spec'.
Return nil if no match."
    (find-font (apply 'font-spec args))))
;;; End eval-and-compile

(provide 'init-pewconfig)
;;; init-pewconfig.el ends here
