;;; init-pewconfig.el --- Pew configurator -*- lexical-binding: t; -*-

;;; Commentary:
;; Code for Pew configurator.

;;; Code:
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
    "Configuration helper.
The available keywords are registered in `pewconfig/keywords'.
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
         ((setq l/section (assoc l/item pewconfig/keywords))
          (setq l/helper (cdr l/section)))
         (t (error "Wrong keyword: %s" l/item))))
      (reverse l/result)))

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
Note: Unlike `pewconfig/set-bind' this macro creates a new map.  It will not be
effective if the map already exists."
    (declare (indent 0))
    (let ((l/map (intern (format "%s-map" (car form))))
          (l/bindings (cdr form)))
      `(let ((lm/key-map (make-sparse-keymap)))
         (dolist (lm/bind ',l/bindings)
           (define-key lm/key-map (pew/tokey (car lm/bind)) (cdr lm/bind)))
         (defvar ,l/map lm/key-map "Created by `pewconfig/set-map'"))))

;;; :bind
  (defmacro pewconfig/set-bind (form)
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
Note: Discouraged `repeat-map' property method in Emacs 28 since it didn't work
well for some reason.  If `repeat-map' needs to be enabled, do:
  (put cmd 'repeat-map map-symbol)"
    (declare (indent 0))
    (let* ((l/cmd (car form))
           (l/cmd-repeat (intern (format "%s-repeat" l/cmd)))
           (l/cmd-doc-string "Created by `pewconfig/set-transient'"))
      `(let ((lm/map (symbol-value (pewconfig/set-map ,form))))
         (defun ,l/cmd (arg)
           "Temporarily activate a transient map.
Normally, when a key binding is invoked in the transient map, it will end and
return the previous key map.
ARG is a prefix argument.  If it is given, the transient map will keep activated
until `C-g' is pressed.
Created by `pewconfig/set-transient'."
           (interactive "P")
           (cond (arg
                  (message "%s activated in repeat mode" ',l/cmd)
                  (set-transient-map lm/map t))
                 (t
                  (message "%s activated" ',l/cmd)
                  (set-transient-map lm/map nil))))
         (defun ,l/cmd-repeat ()
           "Temporarily activate a transient map in repeat mode.
Created by `pewconfig/set-transient'."
           (interactive)
           (,l/cmd :repeat)))))

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
    (let* ((l/var (car form))
           (l/val (cadr form))
           (l/switch (intern (format "switch/%s" l/var))))
      (if (not l/val)
          ;; On-off switch
          `(defun ,l/switch ()
             ,(format "Switch variable `%s' between non-nil and nil.
Created by `pewconfig/set-switch'." l/var)
             (interactive)
             (setq ,l/var (not ,l/var))
             (message "%s: %s" ',l/var (if ,l/var "enabled" "disabled")))
        ;; Rotate switch
        `(defun ,l/switch ()
           ,(format "Switch variable `%s' in the following values
  %S
Created by `pewconfig/set-switch'." l/var l/val)
           (interactive)
           (let* ((lm/list ,l/val)
                  (lm/match (pew/rotate-head lm/list ,l/var 'next)))
             (if lm/match (setq ,l/var (car lm/match))
               ;; Reset the variable if no match
               (setq ,l/var (car lm/list)))
             (message "%s: %s" ',l/var ,l/var))))))

;;; :face
  (defmacro pewconfig/set-face (form)
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
    (let ((l/hook (intern (format "%s-hook" (car form))))
          (l/func (cdr form)))
      `(add-hook ',l/hook #',l/func)))

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
;;; End eval-and-compile

(provide 'init-pewconfig)
;;; init-pewconfig.el ends here
