;;; init-pewcfg.el --- pewcfg -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; The list of keywords
(defvar pewcfg::keywords '(:custom
                           :setq
                           :setq-default
                           :bind
                           :map
                           :transient
                           :switch
                           :face
                           :property
                           :hook
                           :automode
                           :eval
                           :eval-after)
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
  :bind         (KEYMAP [(KEY . DEFINITION) ...])
  :map          (KEYMAP [(KEY . DEFINITION) ...])
  :transient    (COMMAND [(KEY . DEFINITION) ...])
  :switch       (VARIABLE [. (VALUE VALUE ...)])
  :face         (FACE [:KEYWORD VALUE ...])
  :property     (SYMBOL [(PROPERTY . VALUE) ...])
  :hook         (NAME . FUNCTION)
  :automode     (MATCHER . MODE)
  :eval         (SEXP)
  :eval-after   (FEATURE BODY)")

(defvar pewcfg::keyword-normalize-function-format "pewcfg::normalize--%s"
  "The keyword normalize function format.
A normalize functions should take a form and return a list that can be applied
to a keyword handle function by `apply'.")

(defvar pewcfg::keyword-generate-function-format "pewcfg::generate--%s"
  "The keyword generate function format.
Each generate function can have different signatures but it should always return
a list of forms.")

;;; Normalization functions
(defun pewcfg::normalize-identity (form)
  "Like `identity'."
  form)

(defun pewcfg::normalize-pair (form)
  "Convert a pair (cons) to a list."
  (list (car form) (cdr form)))

(defun pewcfg::normalize-single (form)
  "Put a form in a list."
  (list form))

;;; List manipulation functions
(defun pewcfg::until-next-keyword (lst)
  "Return a list that starts with the nearest keyword from the head."
  (named-let pewcfg::--until-next-keyword ((rest lst))
    (cond ((keywordp (car rest))
           rest)
          ((null rest)
           nil)
          (t
           (pewcfg::--until-next-keyword (cdr rest))))))

(defun pewcfg::slice-keyword-segments (lst)
  "Return a list of keyword segments.
Each segment starts with the keyword and all the elements that follow it in the
LST before the next keyword."
  (let* ((start (pewcfg::until-next-keyword lst))
         (next (pewcfg::until-next-keyword (cdr start)))
         (result nil))
    (while start
      (push (butlast start (length next)) result)
      (setq start next)
      (setq next (pewcfg::until-next-keyword (cdr next))))
    (nreverse result)))

;;; Helper functions
(defun pewcfg::tokey (key)
  "Convert KEY to the representation that can be recognized as a keycord.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
  (if (stringp key) (kbd key) key))

;;; Application functions
(defun pewcfg::apply-keyword (keyword &optional forms)
  "Apply FORMS with KEYWORD's handle function.
The result of this function is a list of unevaluated forms."
  (mapcan (lambda (form) (apply (intern (format pewcfg::keyword-generate-function-format keyword))
                                (funcall (intern (format pewcfg::keyword-normalize-function-format keyword))
                                         form)))
          forms))

;;; Main entry
(defmacro pewcfg (&rest args)
  "Main entry of pewcfg utilities.
This one is intended to be used instead of calling generate functions directly.
ARGS is a list of forms.  See the registered helpers from `pewcfg::keywords'
for form definitions.
Typical usage is as follow:
  (pewcfg :KEYWORD FORMS :KEYWORD FORMS ...)"
  (declare (indent 0))
  (if (not (keywordp (car args)))
      (error "Not start with a keyword")
    (cons 'progn (mapcan (lambda (seg)
                           (pewcfg::apply-keyword (car seg) (cdr seg)))
                         (pewcfg::slice-keyword-segments args)))))

;;; :custom
(defalias 'pewcfg::normalize--:custom 'pewcfg::normalize-identity)

(defun pewcfg::generate--:custom (variable value &optional comment)
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
prevents writting settings from this file to the `custom-file'."
  (declare (indent 0))
  `((customize-set-variable ',variable ,value ,comment)))

;;; :setq
(defalias 'pewcfg::normalize--:setq 'pewcfg::normalize-identity)

(defun pewcfg::generate--:setq (variable value &optional comment)
  "Simple wrapper of `setq'.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
COMMENT is not used, which is for compatibility only."
  `((setq ,variable ,value)))

;;; :setq-default
(defalias 'pewcfg::normalize--:setq-default 'pewcfg::normalize-identity)

(defun pewcfg::generate--:setq-default (variable value &optional comment)
  "Simple wrapper of `setq-default'.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
COMMENT is not used, which is for compatibility only."
  `((setq-default ,variable ,value)))

;;; :bind
(defalias 'pewcfg::normalize--:bind 'pewcfg::normalize-identity)

(defun pewcfg::generate--:bind (keymap &rest bindings)
  "Bind keys in an existing KEYMAP.
KEYMAP is a symbol of the keymap.
BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'.
NOTE: Unlike `pewcfg::generate--:map' this macro does not create a new map.  It sets
keybindings in a existing map instead."
  (declare (indent 1))
  `(,@(mapcar (lambda (binding)
                `(define-key ,keymap ,(pewcfg::tokey (car binding)) #',(cdr binding)))
              bindings)
    ,keymap))

;;; :map
(defalias 'pewcfg::normalize--:map 'pewcfg::normalize-identity)

(defun pewcfg::generate--:map (keymap &rest bindings)
  "Create a new KEYMAP and bind keys in it.
KEYMAP is a symbol of the keymap.
BINDINGS is in the same form as in `pewcfg::generate--:bind'.
NOTE: Unlike `pewcfg::generate--:bind' this macro creates a new map.  It will not be
effective if the map already exists."
  (declare (indent 1))
  `((define-prefix-command ',keymap)
    ,@(apply 'pewcfg::generate--:bind keymap bindings)))

;;; :transient
(defalias 'pewcfg::normalize--:transient 'pewcfg::normalize-identity)

(defun pewcfg::generate--:transient (command &rest bindings)
  "Create an interactive COMMAND that enters transient mode when invoked.
COMMAND is a symbol of the command.
BINDINGS is the same with `pewcfg::generate--:bind'.
A map COMMAND-map and an interactive command COMMAND will be created.
Once COMMAND is invoked COMMAND-map will be temporarily activated.
See `set-transient-map'.
NOTE: \\`C-g' and \\`C-h' is preserved and cannot be bound by user.
NOTE: Discouraged `repeat-map' property method in Emacs 28 since it require some
extra work and potentially decrease startup speed.  It needs `repeat-mode' to be
enabled and put the following code for the keymap.
  (map-keymap (lambda (key cmd) (put cmd 'repeat-map 'keymap) keymap)"
  (declare (indent 1))
  (let ((l:cmd-map (intern (format "%s-map" command))))
    `(,@(apply 'pewcfg::generate--:map l:cmd-map bindings)
      ;; Take these two essential bindings.
      (define-key ,l:cmd-map (kbd "C-h") (lambda () (interactive) (,command :repeat)))
      (define-key ,l:cmd-map (kbd "C-g") #'keyboard-quit)
      (defun ,command (arg)
        ,(format "Temporarily activate a transient map.
Normally this is a one-shot invocation meaning the map exits once a key is
pressed (no matter defined in the keymap or not).
However, if a prefix ARG is given, this becomes a repeatable map until \\`C-g'
is pressed.
Alternatively \\`C-h' can be used to transient to the repeat mode while the
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
(defalias 'pewcfg::normalize--:switch 'pewcfg::normalize-pair)

(defun pewcfg::generate--:switch (variable &optional values)
  "Create an interactive command to switch variable from a list of values.
VARIABLE is a symbol of the variable.
VALUES is a list of values that the VARIABLE can be possibly set to.
If VALUES is nil, the VARIABLE will be switch between (t nil) by default.
A new command `switch::VARIABLE' and variable `switch::VARIABLE' will be created.
The variable is used for multiple purposes.  The car of the variable stores the
current index of the list of values that is stored in the cdr."
  (declare (indent 0))
  (let ((l:switch-symbol (intern (format "switch::%s" variable))))
    `((defvar ,l:switch-symbol ',(if values
                                     (cons -1 values)
                                   (cons -1 '(t nil)))
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
(defalias 'pewcfg::normalize--:face 'pewcfg::normalize-identity)

(defun pewcfg::generate--:face (face &rest args)
  "Set FACE attributes.
FACE is a symbol of the face.
ARGS is a plist consists with ATTRIBUTE VALUE pairs.
See `set-face-attribute'."
  (declare (indent 1))
  `((set-face-attribute ',face
                        nil
                        ,@(mapcar (lambda (x) (cond ((keywordp x) x)
                                                    ((symbolp x) (list 'quote x))
                                                    (t x)))
                                  args))))

;;; :property
(defalias 'pewcfg::normalize--:property 'pewcfg::normalize-identity)

(defun pewcfg::generate--:property (symbol &rest properties)
  "Set SYMBOL's PROPERTIES.
Where SYMBOL is the name of the symbol and PROPS is an alist whose element is of
the form:
  (PROP . VAL)
PROP is the symbol of the property and VAL is the value to set with."
  (declare (indent 1))
  `(,@(mapcar (lambda (prop)
                `(put ',symbol ',(car prop) ,(cdr prop)))
              properties)))

;;; :hook
(defalias 'pewcfg::normalize--:hook 'pewcfg::normalize-pair)

(defun pewcfg::generate--:hook (name function)
  "Set a FUNCTION to a hook NAME.
NOTE: NAME does not imply suffix '-hook'."
  (declare (indent 0))
  `((add-hook ',name #',function)))

;;; :automode
(defalias 'pewcfg::normalize--:automode 'pewcfg::normalize-pair)

(defun pewcfg::generate--:automode (matcher mode)
  "Set `auto-mode-alist'.
MATCHER is usually a string of regex.
MODE is a symbol of the mode."
  (declare (indent 0))
  `((add-to-list 'auto-mode-alist ',(cons matcher mode))))

;;; :eval
(defalias 'pewcfg::normalize--:eval 'pewcfg::normalize-single)

(defun pewcfg::generate--:eval (form)
  "Simply return the FORM."
  (declare (indent 0))
  `(,form))

;;; :eval-after
(defalias 'pewcfg::normalize--:eval-after 'pewcfg::normalize-identity)

(defun pewcfg::generate--:eval-after (feature &rest forms)
  "Evaluate FORMS after a FEATURE is loaded."
  (declare (indent 1))
  `((with-eval-after-load ',feature ,@forms)))

(provide 'init-pewcfg)
;;; init-pewcfg.el ends here
