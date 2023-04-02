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
    (named-let pewconfig-inner ((l:keyword-cons nil)
                                (l:args args)
                                (l:running-list '(progn)))
      (if (not l:args)
          ;; Ending
          (reverse l:running-list)
        ;; Otherwise keep processing the elements
        (let ((l:head (assq (car l:args) pewconfig/keywords)))
          (if l:head
              ;; Look up for the next element if the head is a registered cons
              (pewconfig-inner l:head
                               (cdr l:args)
                               l:running-list)
            ;; Otherwise update the list and move to the next element
            (pewconfig-inner l:keyword-cons
                             (cdr l:args)
                             (cons (list (cdr l:keyword-cons) (car l:args))
                                   l:running-list)))))))

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
    (named-let pewconfig/set-map-inner ((l:keymap-symbol (intern (format "%s-map" (car form))))
                                        (l:bindings (cdr form))
                                        (l:running-list (reverse '(let ((ql:keymap (make-sparse-keymap)))))))
      (if l:bindings
          ;; Build the key definition list
          (pewconfig/set-map-inner l:keymap-symbol
                                   (cdr l:bindings)
                                   (cons `(define-key ql:keymap ,(pew::tokey (caar l:bindings)) #',(cdar l:bindings))
                                         l:running-list))
        ;; Add map variable definition at last and return the list
        (reverse (cons `(defvar ,l:keymap-symbol ql:keymap "Created by `pewconfig/set-map'.")
                       l:running-list)))))

;;; :bind
  (defmacro pewconfig/set-bind (form)
    "Bind keys with an existing map.
FORM is of the form:
  (MAP BINDINGS)
Where MAP implies suffix '-map' and BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'."
    (declare (indent 0))
    (named-let pewconfig/set-bind-inner ((l:keymap-symbol (intern (format "%s-map" (car form))))
                                         (l:bindings (cdr form))
                                         (l:running-list '(progn)))
      (if l:bindings
          ;; Build the key definition list
          (pewconfig/set-bind-inner l:keymap-symbol
                                    (cdr l:bindings)
                                    (cons `(define-key ,l:keymap-symbol ,(pew::tokey (caar l:bindings)) #',(cdar l:bindings))
                                          l:running-list))
        ;; Return the list
        (reverse l:running-list))))

;;; :transient
  (defmacro pewconfig/set-transient (form)
    "Create an interactive command that enters transient mode when invoked.
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
    (let ((l:cmd (intern (format "%s" (car form))))
          (l:cmd-repeat (intern (format "%s-repeat" (car form)))))
      `(let ((ql:keymap (symbol-value (pewconfig/set-map ,form))))
         (defun ,l:cmd (arg)
           "Temporarily activate a transient map.
Normally, when a key binding is invoked in the transient map, it will end and
return the previous key map.
ARG is a prefix argument.  If it is given, the transient map will keep activated
until `C-g' is pressed."
           (interactive "P")
           (cond (arg
                  (message "%s activated in repeat mode" ',l:cmd)
                  (set-transient-map ql:keymap t))
                 (t
                  (message "%s activated" ',l:cmd)
                  (set-transient-map ql:keymap nil))))
         (defun ,l:cmd-repeat ()
           "Temporarily activate a transient map in repeat mode."
           (interactive)
           (,l:cmd :repeat)))))

;;; :switch
  (defmacro pewconfig/set-switch (form)
    "Create an interactive command to switch variable from a list of values.
FORM is of the form:
  (VAR . VAL)
Where VAL can be nil or a list.  If VAL is nil then VAR will be switched between
nil and t each time the command is called, otherwise cycle values from the given
list.
A new command 'switch/VAR' will be created as well as a variable with the same
name which stores the list of possible values."
    (declare (indent 0))
    (let ((l:switch-symbol (intern (format "switch/%s" (car form))))
          (l:switch-values (if (cdr form) (cons -1 (cdr form)) (cons -1 '(nil t))))
          (l:switch-var (car form)))
      `(progn
         (defvar ,l:switch-symbol ',l:switch-values
           ,(format  "A list of values used by `%s' command.
The first element is the index which points to the value in the second list
element.  It increments each time the command is call and reset to 0 when
it reaches to the end." l:switch-symbol))
         (defun ,l:switch-symbol ()
           ,(format "Switch the value of variable `%s'.
The values are read from the list `%s'." l:switch-var l:switch-symbol)
           (interactive)
           (setq ,l:switch-var (nth (setcar ,l:switch-symbol
                                            (mod (1+ (car ,l:switch-symbol))
                                                 (length (cdr ,l:switch-symbol))))
                                    (cdr ,l:switch-symbol)))
           (message "Set %s: %s" ',l:switch-var ,l:switch-var)))))

;;; :face
  (defmacro pewconfig/set-face (form)
    "Set face attributes.
FORM is of the form:
  (FACE ARGS)
Where FACE is the name and ARGS comes in pairs ATTRIBUTE VALUE.
See `set-face-attribute'."
    (declare (indent 0))
    `(set-face-attribute ',(car form)
                         nil
                         ,@(mapcar (lambda (x) (cond ((keywordp x) x)
                                                     ((symbolp x) (list 'quote x))
                                                     (t x)))
                                   (cdr form))))

;;; :property
  (defmacro pewconfig/set-property (form)
    "Set symbol's properties.
FORM is of the form:
  (SYM PROPS)
Where SYM is the name of the symbol and PROPS is an alist whose element is of
the form:
  (PROP . VAL)
PROP is the symbol of the property and VAL is the value to set with. "
    (declare (indent 0))
    (named-let pewconfig/set-property-inner ((l:symbol (car form))
                                             (l:props (cdr form))
                                             (l:running-list '(progn)))
      (if l:props
          ;; Build the property definition list
          (pewconfig/set-property-inner l:symbol
                                        (cdr l:props)
                                        (cons `(put ',l:symbol ',(caar l:props) ,(cdar l:props))
                                              l:running-list))
        ;; Return the list
        (reverse l:running-list))))

;;; :hook
  (defmacro pewconfig/set-hook (form)
    "Set function to a hook.
FORM is a cons:
  (HOOK . FUNC)
Where HOOK implies suffix '-hook'."
    (declare (indent 0))
    `(add-hook ',(intern (format "%s-hook" (car form))) #',(cdr form)))

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
  (defmacro pew::swap (a b)
    "Swap values in A and B.
NOTE: A and B must be lvalues."
    `(setq ,a (prog1 ,b (setq ,b ,a))))

  (defun pew::tokey (key)
    "Convert KEY to the representation that can be recognized as a keycord.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
    (if (stringp key) (kbd key) key))

  (defun pew::evenp (num)
    "Determine if NUM is odd."
    (zerop (mod num 2)))

  (defun pew::oddp (num)
    "Determine if NUM is odd."
    (not (pew::evenp num)))

  (defmacro pew::rotate (list &optional reverse)
    "Rotate the LIST by putting the first element to the last.
If REVERSE is non-nil the do it in a opposite way by putting the last element
to the first.
Return a new list or nil if LIST is nil."
    (cond ((not list) nil)
          ((not reverse)
           `(let ((ql:list ,list)) (append (cdr ql:list) (cons (car ql:list) nil))))
          (t
           `(let ((ql:list ,list)) (append (last ql:list) (butlast ql:list))))))

  (defmacro pew::rotate-head (list value &optional next)
    "Rotate LIST and find the matching VALUE.
When NEXT is non-nil the returned list head will be the following value of the
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
         ,(if next '(pew::rotate ql:tail) 'ql:tail))))

  (defun pew::find-font (&rest args)
    "Return a font object is it's found on the current system.
ARGS is the same as the ones defined in `font-spec'.
Return nil if no match."
    (find-font (apply 'font-spec args))))
;;; End eval-and-compile

(provide 'init-pewconfig)
;;; init-pewconfig.el ends here
