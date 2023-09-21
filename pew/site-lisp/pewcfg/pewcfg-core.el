;;; pewcfg-core.el --- pewcfg core components -*- lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

(require 'subr-x)

;;; Variable definitions
(defvar pewcfg::keywords '(:custom
                           :customize
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
  :customize    (VARIABLE VALUE [COMMENT])*
  :setq         (VARIABLE VALUE [COMMENT]) ;; COMMENT has no effect
  :setq-default (VARIABLE VALUE [COMMENT]) ;; COMMENT has no effect
  :bind         (KEYMAP [(KEY . DEFINITION) ...])
  :map          (KEYMAP [(KEY . DEFINITION) ...])
  :transient    (COMMAND [(KEY . DEFINITION) ...])
  :switch       (VARIABLE [. (VALUE VALUE ...)])
  :face         (FACE [:KEYWORD VALUE ...])
  :property     (SYMBOL [(PROPERTY . VALUE) ...])
  :hook         (NAME . FUNCTION)
  :automode     (MATCHER . MODE)
  :eval         (SEXP)
  :eval-after   (FEATURE BODY)

* The difference between ':custom' and ':customize' is that, ':custom' uses a
synthetic theme `pewcfg::custom-theme' to bind variable values to with the
implementation of `custom-theme-set-variables' , while ':customize' is
implemented with `customize-set-variable' to set variables directly.
Both methods are very similar.  They are all capable of keeping the
`custom-file' clean.  ':custom' may be slightly faster than ':customize'.")

(defvar pewcfg::keyword-normalize-function-format "pewcfg::normalize--%s"
  "The keyword normalize function format.
A normalize functions should take a list of forms and make each of them in a
format that can be applied to the generate function by `apply'.")

(defvar pewcfg::keyword-generate-function-format "pewcfg::generate--%s"
  "The keyword generate function format.
Each generate function can have different signatures but it should always return
a list of forms.")

;;; Synthetic theme for custom
(defvar pewcfg::custom-theme 'pewcfg
  "The custom theme symbol used by ':custom'.")

(defun pewcfg::enable-custom-theme ()
  "Declare a synthetic theme for custom variables.
Necessary in order to avoid having those variables saved by custom.el.
After enabling, remove the synthetic theme from the enabled themes, so iterating
over them to disable-all-themes won't disable it."
  (interactive)
  (unless (memq pewcfg::custom-theme custom-known-themes)
    (eval `(deftheme ,pewcfg::custom-theme)))
  (enable-theme pewcfg::custom-theme)
  (setq custom-enabled-themes (remq pewcfg::custom-theme custom-enabled-themes)))

(pewcfg::enable-custom-theme)

;;; Form normalization functions
(defun pewcfg::normalize-identity (form)
  "Return FORM unchanged like `identity'."
  form)

(defun pewcfg::normalize-pair (form)
  "Assume that FORM is a cons cell and convert it to a list."
  (list (car form) (cdr form)))

(defun pewcfg::normalize-first-two (form)
  "Assume that FORM is a list with at least two elements and convert it to a list."
  (list (car form) (cadr form)))

(defun pewcfg::normalize-single (form)
  "Wrap a FORM in a list."
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
  "Return an alist of keyword segments.
Each cons of the returned alist has the keyword as its car and a list of
elements before the next keyword as its cdr."
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
(defun pewcfg::apply-keyword (keyword &rest forms)
  "Apply FORMS with KEYWORD's handler functions.
The FORMS will be processed by the corresponded normalize function first and
then each element of the results will be expanded as the generate function's
input.
The results of this function is a list of unevaluated forms."
  (if (memq keyword pewcfg::keywords)
      (let ((l:gfunc (intern-soft (format pewcfg::keyword-generate-function-format keyword)))
            (l:nfunc (intern-soft (format pewcfg::keyword-normalize-function-format keyword))))
        (mapcan (lambda (form) (apply l:gfunc form))
                (funcall l:nfunc forms)))
    (error "Invalid keyword %S" keyword)))

;;; Main entry
(defmacro pewcfg (&rest args)
  "Main entry of pewcfg utilities.
This one is intended to be used instead of calling generate functions directly.
ARGS is a list of forms.  See the registered helpers from `pewcfg::keywords'
for form definitions.
Typical usage is as follow:
  (pewcfg :KEYWORD FORMS :KEYWORD FORMS ...)"
  (declare (indent 0))
  (if (keywordp (car args))
      (cons 'progn (mapcan (lambda (seg)
                             (apply #'pewcfg::apply-keyword seg))
                           (pewcfg::slice-keyword-segments args)))
    (error "Not start with a keyword")))

;;; :custom
(defun pewcfg::normalize--:custom (forms)
  "Normalize function for ':custom'."
  (list (mapcar (lambda (form)
                  `'(,(elt form 0) ,(elt form 1) nil nil ,(let ((comment (elt form 2)))
                                                            (if comment comment "Set by pewcfg:custom"))))
                forms)))

(defun pewcfg::generate--:custom (&rest args)
  "Bind variable values to a custom theme `pewcfg::custom-theme'."
  `((let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables ',pewcfg::custom-theme ,@args))))

;;; :customize
(defun pewcfg::normalize--:customize (forms)
  "Normalize function for ':customize'."
  (mapcar #'pewcfg::normalize-identity forms))

(defun pewcfg::generate--:customize (variable value &optional comment)
  "Bind a VARIABLE with the VALUE.
Similar to `pewcfg::generate--:custom' while this does not use a custom theme.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
COMMENT is the optional commentary shown in the `customize' interface."
  (declare (indent 0))
  `((customize-set-variable ',variable ,value ,(if comment comment "Set by pewcfg:customize"))))

;;; :setq
(defun pewcfg::normalize--:setq (forms)
  "Normalize function for ':setq'."
  (list (mapcan #'pewcfg::normalize-first-two forms)))

(defun pewcfg::generate--:setq (variable value &rest pairs)
  "Simple wrapper of `setq'.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
PAIRS is the rest of the var-val pairs"
  (declare (indent 0))
  `((setq ,variable ,value ,@pairs)))

;;; :setq-default
(defun pewcfg::normalize--:setq-default (forms)
  "Normalize function for ':setq-default'."
  (list (mapcan #'pewcfg::normalize-first-two forms)))

(defun pewcfg::generate--:setq-default (variable value &rest pairs)
  "Simple wrapper of `setq-default'.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
PAIRS is the rest of the var-val pairs"
  (declare (indent 0))
  `((setq-default ,variable ,value ,@pairs)))

;;; :bind
(defun pewcfg::normalize--:bind (forms)
  "Normalize function for ':bind'."
  (mapcar #'pewcfg::normalize-identity forms))

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
(defun pewcfg::normalize--:map (forms)
  "Normalize function for ':map'."
  (mapcar #'pewcfg::normalize-identity forms))

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
(defun pewcfg::normalize--:transient (forms)
  "Normalize function for ':transient'."
  (mapcar #'pewcfg::normalize-identity forms))

(defun pewcfg::generate--:transient (command &rest bindings)
  "Create an interactive COMMAND that enters transient mode when invoked.
COMMAND is a symbol of the command.
BINDINGS is the same with `pewcfg::generate--:bind'.
A map 'COMMAND-map' and interactive commands 'COMMAND' and 'COMMAND-repeat' will
be created.  See `set-transient-map'.
NOTE: \\`C-g' and \\`C-h' should be preserved.
NOTE: Discouraged `repeat-map' property method in Emacs 28 since it require some
extra work and potentially decrease startup speed.  It needs `repeat-mode' to be
enabled and put the following code for the keymap.
  (map-keymap (lambda (key cmd) (put cmd 'repeat-map 'keymap) keymap)"
  (declare (indent 1))
  (let ((l:command-map (intern (format "%s-map" command)))
        (l:command-repeat (intern (format "%s-repeat" command))))
    `(,@(apply 'pewcfg::generate--:map l:command-map bindings)
      (define-key ,l:command-map ,(kbd "C-g") #'keyboard-quit) ;; Necessary to exit transient mode
      (defun ,command (arg)
        ,(format "Activate map `%s' temporarily.
If prefix ARG is given the map will be activated in a repeatable manner." l:command-map)
        (interactive "P")
        (if arg
            (set-transient-map ,l:command-map
                               (lambda () (not (equal (this-command-keys) (kbd "C-g"))))
                               nil
                               ,(format "%s activated, C-g to exit" l:command-repeat))
          (set-transient-map ,l:command-map nil nil ,(format "%s activated" command))))
      (defun ,l:command-repeat ()
        ,(format "Activate map `%s' in a repeatable manner." l:command-map)
        (interactive)
        (,command :repeat)))))

;;; :switch
(defun pewcfg::normalize--:switch (forms)
  "Normalize function for ':switch'."
  (mapcar #'pewcfg::normalize-pair forms))

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
(defun pewcfg::normalize--:face (forms)
  "Normalize function for ':face'."
  (mapcar #'pewcfg::normalize-identity forms))

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
(defun pewcfg::normalize--:property (forms)
  "Normalize function for ':property'."
  (mapcar #'pewcfg::normalize-identity forms))

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
(defun pewcfg::normalize--:hook (forms)
  "Normalize function for ':hook'."
  (mapcar #'pewcfg::normalize-pair forms))

(defun pewcfg::generate--:hook (name function)
  "Set a FUNCTION to a hook NAME.
NOTE: NAME does not imply suffix '-hook'."
  (declare (indent 0))
  `((add-hook ',name #',function)))

;;; :automode
(defun pewcfg::normalize--:automode (forms)
  "Normalize function for ':automode'."
  (mapcar #'pewcfg::normalize-pair forms))

(defun pewcfg::generate--:automode (matcher mode)
  "Set `auto-mode-alist'.
MATCHER is usually a string of regex.
MODE is a symbol of the mode."
  (declare (indent 0))
  `((add-to-list 'auto-mode-alist ',(cons matcher mode))))

;;; :eval
(defun pewcfg::normalize--:eval (forms)
  "Normalize function for ':eval'."
  (mapcar #'pewcfg::normalize-single forms))

(defun pewcfg::generate--:eval (form)
  "Simply return the FORM."
  (declare (indent 0))
  `(,form))

;;; :eval-after
(defun pewcfg::normalize--:eval-after (forms)
  "Normalize function for ':eval-after'."
  (mapcar #'pewcfg::normalize-identity forms))

(defun pewcfg::generate--:eval-after (feature &rest forms)
  "Evaluate FORMS after a FEATURE is loaded."
  (declare (indent 1))
  `((with-eval-after-load ',feature ,@forms)))

(provide 'pewcfg-core)
;;; pewcfg-core.el ends here
