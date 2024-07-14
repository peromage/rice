;;; pewlib-workspace.el --- Editor manipulations -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; Buffers
(defvar pew::special-buffers
  (list
   ;; VC
   :magit "^ *[Mm]agit"
   :vc "^ *\\*[Vv][Cc]-.*\\*$"
   :ediff "^ *\\*[Ee]diff.*\\*$"
   ;; Interactive
   :shell "^ *\\*\\(\\w+\\s-?\\|\\w+\\)?[Ss]h\\(ell\\)?\\*$"
   :terminal "^ *\\*\\(\\w+\\s-?\\|\\w+\\)?[Tt]erm\\(inal\\)?\\*$"
   :scratch "^ *\\*[Ss]cratch\\*$"
   ;; Org mode
   :org-starred "^ *\\*[Oo]rg "
   :org-src "^ *\\*[Oo]rg [Ss]rc .*\\*$"
   :org-export "^ *\\*[Oo]rg .* [Ee]xport\\*$"
   ;; Edit mode
   :edit-indirect "^ *\\*edit-indirect .*\\*$"
   ;; Man page
   :man "^ *\\*[Mm]an .*\\*$"
   ;; Message and output
   :help "^ *\\*.*\\b[Hh]elp\\*$"
   :message "^ *\\*.*\\b[Mm]essages?\\*$"
   :backtrace "^ *\\*.*\\b[Bb]acktrace\\*$"
   :warning "^ *\\*.*\\b[Ww]arnings?\\*$"
   :log "^ *\\*.*\\b[Ll]og\\*$"
   :compilation "^ *\\*.*\\b[Cc]ompilation\\*$"
   :output "^ *\\*.*\\b[Oo]utput\\*$"
   :command "^ *\\*.*\\b[Cc]ommands?\\*$"
   ;; General
   :starred "^ *\\*.*\\*"
   :non-starred "^ *[^* ]")
  "Special buffer patterns.")

(defvar pew::special-buffer-hidden '(:magit :starred)
  "Buffers that are hiddens for general purposes.")

(defun pew::get-special-buffers (keys &optional concat)
  "Return a list of special buffer regexs based on given KEYS.
The buffer patterns are defined in `pew::special-buffer-regex-plist'.
If CONCAT is non-nil the result is a concatenated regex string."
  (let ((f (lambda (k)
             (let ((v (plist-get pew::special-buffers k)))
               (if v v (error "Invalid key: %S" k))))))
    (if concat
        (mapconcat f (pew::tolist keys) "\\|")
      (mapcar f (pew::tolist keys)))))

(defun pew::special-buffer-p (key name)
  "Check if the given buffer NAME matches special buffer KEY.
The buffer patterns are defined in `pew::special-buffer-regex-plist'."
  (string-match-p (pew::get-special-buffers key 'concat) name))

(defun pew::dired-buffer-p (name)
  "Check if the given buffer NAME is a Dired buffer."
  (eq 'dired-mode (buffer-local-value 'major-mode (get-buffer name))))

(defun pew::next-editing-buffer (&optional backwards)
  "Switch to the next editing buffer.
If BACKWARDS is non-nil switch to the previous one."
  (interactive "P")
  (let ((current-buffer (current-buffer))
        (switch-func (if backwards #'previous-buffer #'next-buffer)))
    (funcall switch-func)
    (while (and (not (eq current-buffer (current-buffer)))
                (or (pew::special-buffer-p pew::special-buffer-hidden (buffer-name))
                    (pew::dired-buffer-p (buffer-name))))
      (funcall switch-func))))

(defun pew::previous-editing-buffer ()
  "Like `pew::next-editing-buffer' but does it backwards."
  (interactive)
  (pew::next-editing-buffer :previous))

(defun pew::close-other-buffers-in-major-mode (mode)
  "Close all other buffers in major MODE but this one."
  (interactive "SMajor mode: ")
  (let ((this-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (if (and (eq mode (buffer-local-value 'major-mode buffer))
               (not (eq this-buffer buffer)))
          (kill-buffer buffer)))))

;;; Windows
(defun pew::side-window-actions (side slot)
  "Return a list of pre-configured side window actions.
See `display-buffer' for property SIDE, SLOT."
  `((display-buffer-reuse-window display-buffer-in-side-window)
    (reusable-frames . t)
    (inhibit-switch-frame . t)
    (window-height . 0.25)
    (side . ,side)
    (slot . ,slot)))

(defun pew::side-window-p (window)
  "Return non-nil if WINDOW is a side window."
  (window-parameter window 'window-side))

(defun pew::side-window-exists-p (&optional side)
  "Return the first side window if there is any, otherwise nil.
If SIDE is given and is one of 'top' 'bottom' 'left' and 'right', check for that
specified side.  If SIDE is nil it means check all sides."
  (window-with-parameter 'window-side side))

(defun pew::normal-window-p (window)
  "Return t if WINDOW is a normal window."
  (not (pew::side-window-p window)))

(defun pew::last-normal-window-p (window)
  "Return t if WINDOW is the last normal window."
  (and (pew::normal-window-p window)
       (= 1 (length (pew::list-normal-windows)))))

(defun pew::list-side-windows ()
  "Return a list of side windows."
  (seq-filter
   (lambda (x) (pew::side-window-p x))
   (window-list)))

(defun pew::list-normal-windows ()
  "Return a list of normal (non-side) windows."
  (seq-filter
   (lambda (x) (pew::normal-window-p x))
   (window-list)))

(defun pew::pop-window-in-new-tab (arg)
  "Pop the current window into a new tab.
If prefix ARG is presented, pop the window without deleting it from the original
place."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (if (and (null arg) (not (pew::last-normal-window-p (selected-window))))
        (delete-window))
    (tab-bar-new-tab) ;; Duplicate current layout
    (select-window (car (pew::list-normal-windows)))
    (switch-to-buffer current-buffer)
    (delete-other-windows)))

(defun pew::pop-window-in-new-tab-persist ()
  "Like `pew::pop-window-in-new-tab' but keep the original window."
  (interactive)
  (pew::pop-window-in-new-tab :persist))

(defun pew::next-window ()
  "Switch to the next window."
  (interactive)
  (other-window 1))

(defun pew::prev-window ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(defun pew::close-window ()
  "Close the current window, or the tab if it is the last normal window."
  (interactive)
  (if (pew::last-normal-window-p (selected-window))
      ;; If there is only one normal window left, close the tab, regardless even
      ;; side windows exist
      (tab-bar-close-tab)
    (delete-window)))

(defun pew::scroll-other-window-page-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window))

(defun pew::scroll-other-window-page-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window-down))

(defun pew::scroll-other-window-line-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window 1))

(defun pew::scroll-other-window-line-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window -1))

(defun pew::recenter-other-window ()
  "Scroll other window one page down."
  (interactive)
  (recenter-other-window))

;;; Tabs
(defun pew::move-tab-next ()
  "Move current tab to the next."
  (interactive)
  (tab-bar-move-tab 1))

(defun pew::move-tab-prev ()
  "Move current tab to the previous."
  (interactive)
  (tab-bar-move-tab -1))

;;; Frames
(defvar pew::frame-opacity-adjust-step 5
  "The amount of opacity changed each time.
Used by `pew::increase-frame-opacity'and `pew::decrease-frame-opacity'.")

(defun pew::set-frame-opacity (val)
  "Set the opacity of the current frame.
VAL is a number between 0 and 100.  0=transparent/100=opaque"
  (interactive "nFrame Opacity [transparent(0) - opaque(100)]: ")
  (let ((value (cond ((> val 100) 100)
                     ((< val 0) 0)
                     (t val))))
    (message "Set Frame opacity: %d%%" value)
    (set-frame-parameter (selected-frame) 'alpha (cons value value))))

(defun pew::increase-frame-opacity ()
  "Increase frame opacity."
  (interactive)
  (pew::set-frame-opacity (+ (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                             pew::frame-opacity-adjust-step)))

(defun pew::decrease-frame-opacity ()
  "Decrease frame opacity."
  (interactive)
  (pew::set-frame-opacity (- (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                             pew::frame-opacity-adjust-step)))

(defun pew::pop-window-in-new-frame (arg)
  "Pop the current window into a new frame.
If prefix ARG is presented, pop the window without deleting it from the original
place."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (if (and (null arg) (not (pew::last-normal-window-p (selected-window))))
        (delete-window))
    (select-frame (make-frame-command))
    (switch-to-buffer current-buffer)
    (delete-other-windows)))

(defun pew::pop-window-in-new-frame-persist ()
  "Like `pew::pop-window-in-new-frame' but keep the original window."
  (interactive)
  (pew::pop-window-in-new-frame :persist))

(provide 'pewlib-workspace)
;;; pewlib-workspace.el ends here
