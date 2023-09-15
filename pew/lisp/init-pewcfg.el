;;; init-pewcfg.el --- pewcfg -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Start eval-and-compile
(eval-and-compile
;;; The list of keywords
  (defvar pewcfg::keywords
    '((:custom       . (pewcfg::with-flattened-form pewcfg::set-custom))
      (:setq         . (pewcfg::with-flattened-form pewcfg::set-setq))
      (:setq-default . (pewcfg::with-flattened-form pewcfg::set-setq-default))
      (:map          . (pewcfg::with-flattened-form pewcfg::set-map))
      (:bind         . (pewcfg::with-flattened-form pewcfg::set-bind))
      (:transient    . (pewcfg::with-flattened-form pewcfg::set-transient))
      (:switch       . (pewcfg::with-flattened-cons pewcfg::set-switch))
      (:face         . (pewcfg::with-flattened-form pewcfg::set-face))
      (:property     . (pewcfg::with-flattened-form pewcfg::set-property))
      (:hook         . (pewcfg::with-flattened-cons pewcfg::set-hook))
      (:automode     . (pewcfg::with-flattened-cons pewcfg::set-automode))
      (:eval         . (pewcfg::with-identical-form pewcfg::set-eval))
      (:eval-after   . (pewcfg::with-flattened-form pewcfg::set-eval-after)))
    "An alist of keywords used by `pewcfg' to specify sections.
Each entry is in the form of
  (KEYWORD . PARTIALLY-APPLIED-FORM)
or
  (KEYWORD . FUNCTION)
where the FUNCTION should accept a form as its parameter.  The structure of the
form can be found below.

List of each keyword's form signature:
  :custom       (VARIABLE VALUE [COMMENT])
  :setq         (VARIABLE VALUE [COMMENT])
  :setq-default (VARIABLE VALUE [COMMENT])
  :map          (KEYMAP [(KEY . DEFINITION) ...])
  :bind         (KEYMAP [(KEY . DEFINITION) ...])
  :transient    (COMMAND [(KEY . DEFINITION) ...])
  :switch       (VARIABLE [. (VALUE VALUE ...)])
  :face         (FACE [:KEYWORD VALUE ...])
  :property     (SYMBOL [(PROPERTY . VALUE) ...])
  :hook         (NAME . FUNCTION)
  :automode     (MATCHER . MODE)
  :eval         (SEXP)
  :eval-after   (FEATURE BODY)
")

;;; Main entry
  (defmacro pewcfg (&rest args)
    "Pew configuration utility.
This one is intended to be used for configuration entry instead of calling the
helper macros directly.
ARGS is a list of forms. See the registered helpers from `pewcfg::keywords'
for form definitions.
Typical usage is as follow:
  (pewcfg :KEYWORD FORMS :KEYWORD FORMS ...)"
    (declare (indent 0))
    (if (not (keywordp (car args)))
        (error "Not start with a keyword")
      (let (l:partial-form)
        `(progn
           ,@(mapcar (lambda (form)
                       (if (not (keywordp form))
                           ;; Complete the expression and expand it
                           `(,@l:partial-form ,form)
                         ;; Find the form according to the keyword
                         (setq l:partial-form (ensure-list (alist-get form pewcfg::keywords)))
                         (if (null l:partial-form)
                             (error "Invalid keyword: %S" form)
                           ;; Insert the keyword as a placeholder
                           form)))
                     args)))))

;;; :custom
  (defun pewcfg::set-custom (variable value &optional comment)
    "Set a VARIABLE that is either a custom or a regular one.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
COMMENT is the optional commentary shown in the `customize' interface.

Underlying implementation uses `customize-set-variable'.

NOTE: Most of vanilla options are defined with `defcustom', which means if they
are set directly by `setq' or `setq-default' they might NOT work as expected.
However, if we use `custom-set-variables' they would work but `custom-file'
would produce a bunch of duplicated settings.  To address this issue, we can use
`customize-set-variable'.  It calls those option setters if they have and also
prevents writting settings from this file to the `custom-file'.
"
    (declare (indent 0))
    `(customize-set-variable ',variable ,value ,comment))

;;; :setq
  (defun pewcfg::set-setq (variable value &optional comment)
    "Simple wrapper of `setq'.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
COMMENT is not used, which is for compatibility only."
    `(setq ,variable ,value))

;;; :setq-default
  (defun pewcfg::set-setq-default (variable value &optional comment)
    "Simple wrapper of `setq-default'.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
COMMENT is not used, which is for compatibility only."
    `(setq-default ,variable ,value))

;;; :bind
  (defun pewcfg::set-bind (keymap &optional bindings)
    "Bind keys in an existing KEYMAP.
KEYMAP is a symbol of the keymap.
BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'.
NOTE: Unlike `pewcfg::set-map' this macro does not create a new map.  It sets
keybindings in a existing map instead."
    (declare (indent 1))
    `(,@(mapcar (lambda (binding)
                  `(define-key ,keymap ,(pewcfg::tokey (car binding)) #',(cdr binding)))
                bindings)
      ,keymap))

;;; :map
  (defun pewcfg::set-map (keymap &optional bindings)
    "Create a new KEYMAP and bind keys in it.
KEYMAP is a symbol of the keymap.
BINDINGS is in the same form as in `pewcfg::set-bind'.
NOTE: Unlike `pewcfg::set-bind' this macro creates a new map.  It will not be
effective if the map already exists."
    (declare (indent 1))
    `((define-prefix-command ',keymap)
      ,@(pewcfg::set-bind keymap bindings)))

;;; :transient
  (defun pewcfg::set-transient (command &optional bindings)
    "Create an interactive COMMAND that enters transient mode when invoked.
COMMAND is a symbol of the command.
BINDINGS is the same with `pewcfg::set-bind'.
A map COMMAND-map and an interactive command COMMAND will be created.
Once COMMAND is invoked COMMAND-map will be temporarily activated.
See `set-transient-map'.
NOTE: 'C-g' and 'C-h' is preserved and cannot be bound by user.
NOTE: Discouraged `repeat-map' property method in Emacs 28 since it require some
extra work and potentially decrease startup speed.  It needs `repeat-mode' to be
enabled and put the following code for the keymap.
  (map-keymap (lambda (key cmd) (put cmd 'repeat-map 'keymap) keymap)"
    (declare (indent 1))
    (let ((l:cmd-map (intern (format "%s-map" command))))
      `(,@(pewcfg::set-map l:cmd-map bindings)
        ;; Take these two essential bindings.
        (define-key ,l:cmd-map (kbd "C-h") (lambda () (interactive) (,command :repeat)))
        (define-key ,l:cmd-map (kbd "C-g") #'keyboard-quit)
        (defun ,command (arg)
          ,(format "Temporarily activate a transient map.
Normally this is a one-shot invocation meaning the map exits once a key is
pressed (no matter defined in the keymap or not).
However, if a prefix ARG is given, this becomes a repeatable map until 'C-g'
is pressed.
Alternatively 'C-h' can be used to transient to the repeat mode while the
transient map is active.
Do not attempt to use C-h multiple times.
The keymap is defined in `%s'." l:cmd-map)
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
  (defun pewcfg::set-switch (variable &optional values)
    "Create an interactive command to switch variable from a list of values.
VARIABLE is a symbol of the variable.
VALUES is a list of values that the VARIABLE can be possibly set to.
If VALUES is nil, the VARIABLE will be switch between (nil t) by default.
A new command 'switch::VARIABLE' will be created as well as a variable with
the same name which stores VALUES"
    (declare (indent 0))
    (let ((l:switch-symbol (intern (format "switch::%s" variable)))
          (l:switch-values (if values (cons -1 values) (cons -1 '(nil t)))))
      `(progn
         (defvar ,l:switch-symbol ',l:switch-values
           ,(format  "A list of values used by `%s' command.
The first element is the index which points to the current value.  The index
cycles through the list each the switch command is called." l:switch-symbol))
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
  (defun pewcfg::set-face (face &optional args)
    "Set FACE attributes.
FACE is a symbol of the face.
ARGS is a plist consists with ATTRIBUTE VALUE pairs.
See `set-face-attribute'."
    (declare (indent 1))
    `(set-face-attribute ',face
                         nil
                         ,@(mapcar (lambda (x) (cond ((keywordp x) x)
                                                     ((symbolp x) (list 'quote x))
                                                     (t x)))
                                   args)))

;;; :property
  (defun pewcfg::set-property (symbol &optional properties)
    "Set SYMBOL's PROPERTIES.
Where SYMBOL is the name of the symbol and PROPS is an alist whose element is of
the form:
  (PROP . VAL)
PROP is the symbol of the property and VAL is the value to set with. "
    (declare (indent 1))
    `(,@(mapcar (lambda (prop)
                  `(put ',symbol ',(car prop) ,(cdr prop)))
                properties)))

;;; :hook
  (defun pewcfg::set-hook (name function)
    "Set a FUNCTION to a hook NAME.
NOTE: NAME does not imply suffix '-hook'."
    (declare (indent 0))
    `(add-hook ',name #',function))

;;; :automode
  (defun pewcfg::set-automode (matcher mode)
    "Set `auto-mode-alist'.
MATCHER is usually a string of regex.
MODE is a symbol of the mode."
    (declare (indent 0))
    `(add-to-list 'auto-mode-alist ',(cons matcher mode)))

;;; :eval
  (defun pewcfg::set-eval (form)
    "Simply return the FORM."
    (declare (indent 0))
    form)

;;; :eval-after
  (defun pewcfg::set-eval-after (feature &optional forms)
    "Evaluate FORMS after a FEATURE is loaded."
    (declare (indent 1))
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
