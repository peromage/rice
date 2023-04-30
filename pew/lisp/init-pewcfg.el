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
    "An alist of keywords used by `pewcfg' to specify sections.
Each entry is in the form of
  (KEYWORD . (EXPANSION-MACRO . FORM-PROCESSOR)

List of each keyword's form signature:
  :custom      (VARIABLE VALUE [COMMENT])
  :map         (KEYMAP [(KEY . DEFINITION) ...])
  :bind        (KEYMAP [(KEY . DEFINITION) ...])
  :transient   (COMMAND [(KEY . DEFINITION) ...])
  :switch      (VARIABLE [. (VALUE VALUE ...)])
  :face        (FACE [:KEYWORD VALUE ...])
  :property    (SYMBOL [(PROPERTY . VALUE) ...])
  :hook        (NAME . FUNCTION)
  :automode    (MATCHER . MODE)
  :eval        (SEXP)
  :eval-after  (FEATURE BODY)
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
Underlying implementation uses `customize-set-variable'."
    (declare (indent 0))
    `(customize-set-variable ',variable ,value ,comment))

;;; :bind
  (defmacro pewcfg::set-bind (keymap &rest bindings)
    "Bind keys in an existing KEYMAP.
BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'.
NOTE: Unlike `pewcfg::set-map' this macro does not create a new map.  It sets
keybindings in a existing map instead."
    (declare (indent 0))
    `(progn
       ,@(mapcar (lambda (binding)
                   `(define-key ,keymap ,(pewcfg::tokey (car binding)) #',(cdr binding)))
                 bindings)
       ,keymap))

;;; :map
  (defmacro pewcfg::set-map (keymap &rest bindings)
    "Create a new KEYMAP and bind keys in it.
BINDINGS is the same with `pewcfg::set-bind'.
NOTE: Unlike `pewcfg::set-bind' this macro creates a new map.  It will not be
effective if the map already exists."
    (declare (indent 0))
    `(progn
       (define-prefix-command ',keymap)
       (pewcfg::set-bind ,keymap ,@bindings)))

;;; :transient
  (defmacro pewcfg::set-transient (command &rest bindings)
    "Create an interactive COMMAND that enters transient mode when invoked.
BINDINGS is the same with `pewcfg::set-bind'.
A map COMMAND-map and an interactive command COMMAND will be created.
Once COMMAND is invoked COMMAND-map will be temporarily activated.
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
However, if a prefix ARG is given, this becomes a repeatable map until 'C-g'
is pressed.
Alternatively 'C-h' can be used to transient to the repeat mode while the
transient map is active.
Do not attempt to use C-h multiple times.
NOTE: 'C-g' and 'C-h' is preserved and cannot be bound by user."
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
VALUES is a list of values that the VARIABLE can be possibly set to.
If VALUES is nil, the VARIABLE will be switch between (nil t) by default.
A new command 'switch/VARIABLE' will be created as well as a variable with
the same name which stores VALUES"
    (declare (indent 0))
    (let ((l:switch-symbol (intern (format "switch/%s" variable)))
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
  (defmacro pewcfg::set-face (face &rest args)
    "Set FACE attributes.
ARGS is in pairs of ATTRIBUTE VALUE.
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
    "Set SYMBOL's PROPERTIES.
Where SYMBOL is the name of the symbol and PROPS is an alist whose element is of
the form:
  (PROP . VAL)
PROP is the symbol of the property and VAL is the value to set with. "
    (declare (indent 0))
    `(progn
       ,@(mapcar (lambda (prop)
                   `(put ',symbol ',(car prop) ,(cdr prop)))
                 properties)))

;;; :hook
  (defmacro pewcfg::set-hook (name function)
    "Set a FUNCTION to a hook NAME.
NOTE: NAME implies suffix '-hook'."
    (declare (indent 0))
    `(add-hook ',(intern (format "%s-hook" name)) #',function))

;;; :automode
  (defmacro pewcfg::set-automode (matcher mode)
    "Set `auto-mode-alist'.
MATCHER is usually a string of regex.
MODE is a symbol of modes."
    (declare (indent 0))
    `(add-to-list 'auto-mode-alist ',(cons matcher mode)))

;;; :eval
  (defmacro pewcfg::set-eval (form)
    "Simply evaluate FORM and nothing else."
    (declare (indent 0))
    form)

;;; :eval-after
  (defmacro pewcfg::set-eval-after (feature &rest forms)
    "Evaluate FORMS after a FEATURE is loaded."
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
