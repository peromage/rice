;;; init-pewcfg.el --- Pew configurator -*- lexical-binding: t; -*-

;;; Commentary:
;; Code for Pew configurator.

;;; Code:
;;; Start eval-and-compile
(eval-and-compile
;;; The list of keywords
  (defvar pewcfg::keywords
    '((:custom . pewcfg::set-custom)
      (:map . pewcfg::set-map)
      (:bind . pewcfg::set-bind)
      (:transient . pewcfg::set-transient)
      (:switch . pewcfg::set-switch)
      (:face . pewcfg::set-face)
      (:property . pewcfg::set-property)
      (:hook . pewcfg::set-hook)
      (:automode . pewcfg::set-automode)
      (:eval . pewcfg::set-eval))
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
    (named-let pewcfg-inner ((l:keyword-cons nil)
                             (l:args args)
                             (l:running-list '(progn)))
      (if (not l:args)
          ;; Ending
          (reverse l:running-list)
        ;; Otherwise keep processing the elements
        (let ((l:head (assq (car l:args) pewcfg::keywords)))
          (if l:head
              ;; Look up for the next element if the head is a registered cons
              (pewcfg-inner l:head
                            (cdr l:args)
                            l:running-list)
            ;; Otherwise update the list and move to the next element
            (pewcfg-inner l:keyword-cons
                          (cdr l:args)
                          (cons (list (cdr l:keyword-cons) (car l:args))
                                l:running-list)))))))

;;; :custom
  (defmacro pewcfg::set-custom (form)
    "Set custom variables or regular variables.
FORM is of the form:
  (VAR VALUE [COMMENT])
Underlying implementation uses `customize-set-variable'."
    (declare (indent 0))
    `(customize-set-variable ',(nth 0 form) ,(nth 1 form) ,(nth 2 form)))

;;; :map
  (defmacro pewcfg::set-map (form)
    "Create a new map and bind keys with it.
FORM is of the form:
  (MAP BINDINGS)
Where MAP implies suffix '-map' and BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'.
NOTE: Unlike `pewcfg::set-bind' this macro creates a new map.  It will not be
effective if the map already exists."
    (declare (indent 0))
    (named-let pewcfg::set-map-inner ((l:keymap-symbol (intern (format "%s-map" (car form))))
                                      (l:bindings (cdr form))
                                      (l:running-list (reverse '(let ((ql:keymap (make-sparse-keymap)))))))
      (if l:bindings
          ;; Build the key definition list
          (pewcfg::set-map-inner l:keymap-symbol
                                 (cdr l:bindings)
                                 (cons `(define-key ql:keymap ,(pewcfg::tokey (caar l:bindings)) #',(cdar l:bindings))
                                       l:running-list))
        ;; Add map variable definition at last and return the list
        (reverse (cons `(defvar ,l:keymap-symbol ql:keymap "Created by `pewcfg::set-map'.")
                       l:running-list)))))

;;; :bind
  (defmacro pewcfg::set-bind (form)
    "Bind keys with an existing map.
FORM is of the form:
  (MAP BINDINGS)
Where MAP implies suffix '-map' and BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `define-key'."
    (declare (indent 0))
    (named-let pewcfg::set-bind-inner ((l:keymap-symbol (intern (format "%s-map" (car form))))
                                       (l:bindings (cdr form))
                                       (l:running-list '(progn)))
      (if l:bindings
          ;; Build the key definition list
          (pewcfg::set-bind-inner l:keymap-symbol
                                  (cdr l:bindings)
                                  (cons `(define-key ,l:keymap-symbol ,(pewcfg::tokey (caar l:bindings)) #',(cdar l:bindings))
                                        l:running-list))
        ;; Return the list
        (reverse l:running-list))))

;;; :transient
  (defmacro pewcfg::set-transient (form)
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
      `(let ((ql:keymap (symbol-value (pewcfg::set-map ,form))))
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
  (defmacro pewcfg::set-switch (form)
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
  (defmacro pewcfg::set-face (form)
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
  (defmacro pewcfg::set-property (form)
    "Set symbol's properties.
FORM is of the form:
  (SYM PROPS)
Where SYM is the name of the symbol and PROPS is an alist whose element is of
the form:
  (PROP . VAL)
PROP is the symbol of the property and VAL is the value to set with. "
    (declare (indent 0))
    (named-let pewcfg::set-property-inner ((l:symbol (car form))
                                           (l:props (cdr form))
                                           (l:running-list '(progn)))
      (if l:props
          ;; Build the property definition list
          (pewcfg::set-property-inner l:symbol
                                      (cdr l:props)
                                      (cons `(put ',l:symbol ',(caar l:props) ,(cdar l:props))
                                            l:running-list))
        ;; Return the list
        (reverse l:running-list))))

;;; :hook
  (defmacro pewcfg::set-hook (form)
    "Set function to a hook.
FORM is a cons:
  (HOOK . FUNC)
Where HOOK implies suffix '-hook'."
    (declare (indent 0))
    `(add-hook ',(intern (format "%s-hook" (car form))) #',(cdr form)))

;;; :automode
  (defmacro pewcfg::set-automode (form)
    "Set `auto-mode-alist'.
FORM is a cons:
  (MATCHER . MODE)
Where MATCHER is usually a string of regex."
    (declare (indent 0))
    `(add-to-list 'auto-mode-alist ',form))

;;; :eval
  (defmacro pewcfg::set-eval (form)
    "Simply evaluate FORM and nothing else."
    form)

;;; Utility macros/functions
  (defun pewcfg::tokey (key)
    "Convert KEY to the representation that can be recognized as a keycord.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
    (if (stringp key) (kbd key) key)))
;;; End eval-and-compile

(provide 'init-pewcfg)
;;; init-pewcfg.el ends here
