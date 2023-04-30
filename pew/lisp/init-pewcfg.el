;;; init-pewcfg.el --- Pew configurator -*- lexical-binding: t; -*-

;;; Commentary:
;; Code for Pew configurator.

;;; Code:
;;; Start eval-and-compile
(eval-and-compile
;;; The list of keywords
  (defvar pewcfg::keywords
    '((:custom     . (pewcfg::set-custom . pewcfg::with-flattened-form))
      (:map        . (pewcfg::set-map . pewcfg::with-flattened-form))
      (:bind       . (pewcfg::set-bind . pewcfg::with-flattened-form))
      (:transient  . (pewcfg::set-transient . pewcfg::with-flattened-form))
      (:switch     . (pewcfg::set-switch . pewcfg::with-flattened-cons))
      (:face       . (pewcfg::set-face . pewcfg::with-flattened-form))
      (:property   . (pewcfg::set-property . pewcfg::with-flattened-form))
      (:hook       . (pewcfg::set-hook . pewcfg::with-flattened-cons))
      (:automode   . (pewcfg::set-automode . pewcfg::with-flattened-cons))
      (:eval       . (pewcfg::set-eval . pewcfg::with-identical-form))
      (:eval-after . (pewcfg::set-eval-after . pewcfg::with-flattened-form)))
    "An alist of keywords used in `pewcfg' to specify sections.
The value of each element is the expansion helper of that section.")

;;; Main entry
  (defmacro pewcfg (&rest args)
    "Pew configuration utility.
This one is intended to be used for configuration entry instead of calling the
helper macros directly.
ARGS is a list of forms. See the registered helpers from `pewcfg::keywords'
for form definitions.
Typical usage is as follow:
  (pewcfg
    :custom
    (inhibit-startup-buffer-menu t)
    :bind
    (global
      (\"C-x C-d\" . dired-jump))
    ...) "
    (declare (indent 0))
    (let (l:stored l:entry)
      `(progn
         ,@(mapcar (lambda (form)
                     (setq l:entry (assq form pewcfg::keywords))
                     (if (null l:entry)
                         ;; Expand form with corresponded macro
                         (list (cdr l:stored) (car l:stored) form)
                       ;; Update current function
                       (setq l:stored (cdr l:entry))
                       (car l:entry)))
                   args))))

;;; :custom
  (defmacro pewcfg::set-custom (variable value &optional comment)
    "Set custom variables or regular variables.
FORM is of the form:
  (VAR VALUE [COMMENT])
Underlying implementation uses `customize-set-variable'."
    (declare (indent 0))
    `(customize-set-variable ',variable ,value ,comment))

;;; :bind
  (defmacro pewcfg::set-bind (keymap &rest bindings)
    "Bind keys with an existing map.
FORM is of the form:
  (MAP BINDINGS)
BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'.
NOTE: Unlike `pewcfg::set-map' this macro does not create a new map.  It set key
bindings in a existing map instead."
    (declare (indent 0))
    `(progn
       ,@(mapcar (lambda (binding)
                   `(define-key ,keymap ,(pewcfg::tokey (car binding)) #',(cdr binding)))
                 bindings)
       ,keymap))

;;; :map
  (defmacro pewcfg::set-map (keymap &rest bindings)
    "Create a new map and bind keys with it.
FORM is of the form:
  (MAP BINDINGS)
BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'.
NOTE: Unlike `pewcfg::set-bind' this macro creates a new map.  It will not be
effective if the map already exists."
    (declare (indent 0))
    `(progn
       (define-prefix-command ',keymap)
       (pewcfg::set-bind ,keymap ,@bindings)))

;;; :transient
  (defmacro pewcfg::set-transient (command &rest bindings)
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
NOTE: Discouraged `repeat-map' property method in Emacs 28 since it require some
extra work and potentially decrease startup speed.  It needs `repeat-mode' to be
enabled and put the following code for the keymap.
  (map-keymap (lambda (key cmd) (put cmd 'repeat-map 'keymap) keymap)"
    (declare (indent 0))
    (let ((l:cmd-map (intern (format "%s-map" command))))
      `(progn
         (pewcfg::set-map ,l:cmd-map ,@bindings)
         ;; Take these two essential bindings.
         (define-key ,l:cmd-map (kbd "C-h") (lambda () (interactive) (,command :repeat)))
         (define-key ,l:cmd-map (kbd "C-g") #'keyboard-quit)
         (defun ,command (arg)
           "Temporarily activate a transient map.
Normally this is a one-shot invocation meaning the map exits once a key is
pressed (no matter defined in the keymap or not).
However, if a prefix ARG is given, this becomes a repeatable map until C-g
is pressed.
Or C-h can be used to transient to the repeat mode while the transient map is
active.
Do not attempt to use C-h multiple times.
NOTE: C-g and C-h will be overridden even if they are defined by user."
           (interactive "P")
           (cond (arg
                  (message "%s activated in repeat mode" ',command)
                  (set-transient-map ,l:cmd-map (lambda ()
                                                  (cond ((equal (this-command-keys) (kbd "C-g"))
                                                         (message "%s repeat mode exited" ',command)
                                                         nil)
                                                        (t
                                                         (message "%s repeat mode, to exit C-g" ',command)
                                                         t)))))
                 (t
                  (message "%s activated" ',command)
                  (set-transient-map ,l:cmd-map nil)))))))

;;; :switch
  (defmacro pewcfg::set-switch (variable &optional values)
    "Create an interactive command to switch variable from a list of values.
FORM is of the form:
  (VAR . VAL)
Where VAL can be nil or a list.  If VAL is nil then VAR will be switched between
nil and t each time the command is called, otherwise cycle values from the given
list.
A new command 'switch/VAR' will be created as well as a variable with the same
name which stores the list of possible values."
    (declare (indent 0))
    (let ((l:switch-symbol (intern (format "switch/%s" variable)))
          (l:switch-values (if values (cons -1 values) (cons -1 '(nil t)))))
      `(progn
         (defvar ,l:switch-symbol ',l:switch-values
           ,(format  "A list of values used by `%s' command.
The first element is the index which points to the value in the second list
element.  It increments each time the command is call and reset to 0 when
it reaches to the end." l:switch-symbol))
         (defun ,l:switch-symbol ()
           ,(format "Switch the value of variable `%s'.
The values are read from the list `%s'." variable l:switch-symbol)
           (interactive)
           (setq ,variable (nth (setcar ,l:switch-symbol
                                        (mod (1+ (car ,l:switch-symbol))
                                             (length (cdr ,l:switch-symbol))))
                                (cdr ,l:switch-symbol)))
           (message "Set %s: %s" ',variable ,variable)))))

;;; :face
  (defmacro pewcfg::set-face (face &rest args)
    "Set face attributes.
FORM is of the form:
  (FACE ARGS)
Where FACE is the name and ARGS comes in pairs ATTRIBUTE VALUE.
See `set-face-attribute'."
    (declare (indent 0))
    `(set-face-attribute ',face
                         nil
                         ,@(mapcar (lambda (x) (cond ((keywordp x) x)
                                                     ((symbolp x) (list 'quote x))
                                                     (t x)))
                                   args)))

;;; :property
  (defmacro pewcfg::set-property (symbol &rest properties)
    "Set symbol's properties.
FORM is of the form:
  (SYM PROPS)
Where SYM is the name of the symbol and PROPS is an alist whose element is of
the form:
  (PROP . VAL)
PROP is the symbol of the property and VAL is the value to set with. "
    (declare (indent 0))
    `(progn
       ,@(mapcar (lambda (prop)
                   `(put ',symbol ',(car prop) ,(cdr prop)))
                 properties)))

;;; :hook
  (defmacro pewcfg::set-hook (name func)
    "Set function to a hook.
FORM is a cons:
  (HOOK . FUNC)
Where HOOK implies suffix '-hook'."
    (declare (indent 0))
    `(add-hook ',(intern (format "%s-hook" name)) #',func))

;;; :automode
  (defmacro pewcfg::set-automode (matcher mode)
    "Set `auto-mode-alist'.
FORM is a cons:
  (MATCHER . MODE)
Where MATCHER is usually a string of regex."
    (declare (indent 0))
    `(add-to-list 'auto-mode-alist ',(cons matcher mode)))

;;; :eval
  (defmacro pewcfg::set-eval (form)
    "Simply evaluate FORM and nothing else."
    (declare (indent 0))
    form)

;;; :eval-after
  (defmacro pewcfg::set-eval-after (feature &rest forms)
    "Evaluate forms after a feature is loaded.
FORM is of form:
  (FEATURE FORMS)
Where FORMS is a list of forms. "
    (declare (indent 0))
    `(with-eval-after-load ',feature ,@forms))

;;; Utility macros/functions
  (defmacro pewcfg::with-flattened-form (callable form)
    "Invoke CALLABLE with its argument list by flattening FORM."
    `(,callable ,@form))

  (defmacro pewcfg::with-flattened-cons (callable cons)
    "Invoke CALLABLE with its argument list by flattening CONS."
    `(,callable ,(car cons) ,(cdr cons)))

  (defmacro pewcfg::with-identical-form (callable form)
    "Invoke CALLABLE with FORM unchanged."
    `(,callable ,form))

  (defun pewcfg::tokey (key)
    "Convert KEY to the representation that can be recognized as a keycord.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
    (if (stringp key) (kbd key) key)))
;;; End eval-and-compile

(provide 'init-pewcfg)
;;; init-pewcfg.el ends here
