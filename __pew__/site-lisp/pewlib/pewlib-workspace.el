;;; pewlib-workspace.el --- Editor manipulations -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; Buffers
(defvar pewlib::special-buffers
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
   ;; Info page
   :man "^ *\\*[Mm]an .*\\*$"
   :eldoc "^\\*eldoc\\*$"
   :tree-sitter-explorer "^\\*tree-sitter explorer.*\\*$"
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

(defvar pewlib::special-buffer-hidden '(:magit :starred)
  "Buffers that are hiddens for general purposes.")

(defun pewlib::get-special-buffers (keys &optional concat)
  "Return a list of special buffer regexs based on given KEYS.
The buffer patterns are defined in `pewlib::special-buffer-regex-plist'.
If CONCAT is non-nil the result is a concatenated regex string."
  (let ((f (lambda (k)
             (let ((v (plist-get pewlib::special-buffers k)))
               (if v v (error "Invalid key: %S" k))))))
    (if concat
        (mapconcat f (pewlib::tolist keys) "\\|")
      (mapcar f (pewlib::tolist keys)))))

(defun pewlib::special-buffer-p (key name)
  "Check if the given buffer NAME matches special buffer KEY.
The buffer patterns are defined in `pewlib::special-buffer-regex-plist'."
  (string-match-p (pewlib::get-special-buffers key 'concat) name))

(defun pewlib::next-editing-buffer (&optional backwards)
  "Switch to the next editing buffer.
If BACKWARDS is non-nil switch to the previous one."
  (interactive "P")
  (let ((current-buffer (current-buffer))
        (switch-func (if backwards #'previous-buffer #'next-buffer)))
    (funcall switch-func)
    (while (and (not (eq current-buffer (current-buffer)))
                (or (pewlib::special-buffer-p pewlib::special-buffer-hidden (buffer-name))
                    (pewlib::dired-buffer-p (buffer-name))))
      (funcall switch-func))))

(defun pewlib::previous-editing-buffer ()
  "Like `pewlib::next-editing-buffer' but does it backwards."
  (interactive)
  (pewlib::next-editing-buffer :previous))

(defun pewlib::close-other-buffers-in-major-mode (mode)
  "Close all other buffers in major MODE but this one."
  (interactive "SMajor mode: ")
  (let ((this-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (if (and (eq mode (buffer-local-value 'major-mode buffer))
               (not (eq this-buffer buffer)))
          (kill-buffer buffer)))))

;;; Windows
(defun pewlib::reuse-window-in-buffer ()
  "Make new spawned windows atttempt to reuse current ones.
This is usually useful in some major modes like `grep-mode'."
  (setq-local display-buffer-base-action '((display-buffer-reuse-window
                                            display-buffer-use-some-window))
              display-buffer-alist nil))

(defun pewlib::side-window-actions (side slot)
  "Return a list of pre-configured side window actions.
See `display-buffer' for property SIDE, SLOT."
  `((display-buffer-reuse-window display-buffer-in-side-window)
    (reusable-frames . t)
    (inhibit-switch-frame . t)
    (window-height . 0.25)
    (side . ,side)
    (slot . ,slot)))

(defun pewlib::side-window-p (window)
  "Return non-nil if WINDOW is a side window."
  (window-parameter window 'window-side))

(defun pewlib::side-window-exists-p (&optional side)
  "Return the first side window if there is any, otherwise nil.
If SIDE is given and is one of 'top' 'bottom' 'left' and 'right', check for that
specified side.  If SIDE is nil it means check all sides."
  (window-with-parameter 'window-side side))

(defun pewlib::normal-window-p (window)
  "Return t if WINDOW is a normal window."
  (not (pewlib::side-window-p window)))

(defun pewlib::last-normal-window-p (window)
  "Return t if WINDOW is the last normal window."
  (and (pewlib::normal-window-p window)
       (= 1 (length (pewlib::list-normal-windows)))))

(defun pewlib::list-side-windows ()
  "Return a list of side windows."
  (seq-filter
   (lambda (x) (pewlib::side-window-p x))
   (window-list)))

(defun pewlib::list-normal-windows ()
  "Return a list of normal (non-side) windows."
  (seq-filter
   (lambda (x) (pewlib::normal-window-p x))
   (window-list)))

(defun pewlib::pop-window-in-new-tab (arg)
  "Pop the current window into a new tab.
If prefix ARG is presented, pop the window without deleting it from the original
place."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (if (and (null arg) (not (pewlib::last-normal-window-p (selected-window))))
        (delete-window))
    (tab-bar-new-tab) ;; Duplicate current layout
    (select-window (car (pewlib::list-normal-windows)))
    (switch-to-buffer current-buffer)
    (delete-other-windows)))

(defun pewlib::pop-window-in-new-tab-persist ()
  "Like `pewlib::pop-window-in-new-tab' but keep the original window."
  (interactive)
  (pewlib::pop-window-in-new-tab :persist))

(defun pewlib::next-window ()
  "Switch to the next window."
  (interactive)
  (other-window 1))

(defun pewlib::prev-window ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(defun pewlib::close-window ()
  "Close the current window, or the tab if it is the last normal window."
  (interactive)
  (if (pewlib::last-normal-window-p (selected-window))
      ;; If there is only one normal window left, close the tab, regardless even
      ;; side windows exist
      (tab-bar-close-tab)
    (delete-window)))

(defun pewlib::scroll-other-window-page-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window))

(defun pewlib::scroll-other-window-page-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window-down))

(defun pewlib::scroll-other-window-line-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window 1))

(defun pewlib::scroll-other-window-line-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window -1))

(defun pewlib::recenter-other-window ()
  "Scroll other window one page down."
  (interactive)
  (recenter-other-window))

;;; Tabs
(defun pewlib::move-tab-next ()
  "Move current tab to the next."
  (interactive)
  (tab-bar-move-tab 1))

(defun pewlib::move-tab-prev ()
  "Move current tab to the previous."
  (interactive)
  (tab-bar-move-tab -1))

;;; Frames
(defvar pewlib::frame-opacity-adjust-step 5
  "The amount of opacity changed each time.
Used by `pewlib::increase-frame-opacity'and `pewlib::decrease-frame-opacity'.")

(defun pewlib::set-frame-opacity (val)
  "Set the opacity of the current frame.
VAL is a number between 0 and 100.  0=transparent/100=opaque"
  (interactive "nFrame Opacity [transparent(0) - opaque(100)]: ")
  (let ((value (cond ((> val 100) 100)
                     ((< val 0) 0)
                     (t val))))
    (message "Set Frame opacity: %d%%" value)
    (set-frame-parameter (selected-frame) 'alpha (cons value value))))

(defun pewlib::increase-frame-opacity ()
  "Increase frame opacity."
  (interactive)
  (pewlib::set-frame-opacity (+ (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                                pewlib::frame-opacity-adjust-step)))

(defun pewlib::decrease-frame-opacity ()
  "Decrease frame opacity."
  (interactive)
  (pewlib::set-frame-opacity (- (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                                pewlib::frame-opacity-adjust-step)))

(defun pewlib::pop-window-in-new-frame (arg)
  "Pop the current window into a new frame.
If prefix ARG is presented, pop the window without deleting it from the original
place."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (if (and (null arg) (not (pewlib::last-normal-window-p (selected-window))))
        (delete-window))
    (select-frame (make-frame-command))
    (switch-to-buffer current-buffer)
    (delete-other-windows)))

(defun pewlib::pop-window-in-new-frame-persist ()
  "Like `pewlib::pop-window-in-new-frame' but keep the original window."
  (interactive)
  (pewlib::pop-window-in-new-frame :persist))

(provide 'pewlib-workspace)
;;; pewlib-workspace.el ends here
