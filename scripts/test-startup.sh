#!/bin/sh

# Working directory check
if [ ! -f init.el ]; then
    echo "Not in the config root directory"
    exit 1
fi

echo "Starting Emasc..."

emacs -nw --batch --eval \
      '(progn
         (require (quote url-vars))
         (let ((debug-on-error t)
               (url-show-status nil)
               (user-emacs-directory default-directory)
               (user-init-file (expand-file-name "init.el" default-directory))
               (load-path (delq default-directory load-path)))
           (load-file user-init-file)
           (run-hooks (quote after-init-hook))
           (run-hooks (quote emacs-startup-hook))))'

ec=$?

if [ $ec -eq 0 ]; then
    echo "Successfully started Emacs"
else
    echo "Failed to start Emacs" 1>&2
fi

exit $ec
