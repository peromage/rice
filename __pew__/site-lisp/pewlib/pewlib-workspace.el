;;; pewlib-workspace.el --- Editor manipulations -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; Buffers
(defvar /ns/buffer-regex-plist
  (let ((star "^ *\\*%s\\* *$")
        (leading-star "^ *\\*%s"))
    ;; Align regex  :[a-z-]+\(\s-*\)\((\|"\)
    (list
     ;; Generic
     :starred              (format star ".*")
     :non-starred          "^ *[^* ]"
     :with-leading-star    (format star ".*")
     :with-leading-space   "^ +"
     ;; Extension buffers
     :magit                "^ *[Mm]agit"
     :vc                   (format star "[Vv][Cc]-.*")
     :ediff                (format star "[Ee]diff.*")
     :shell                (format star "\\w* *[Ss]hell")
     :terminal             (format star "\\w* *[Tt]erm\\(inal\\)?")
     :org-starred          (format star "[Oo]rg ?.*")
     :edit-indirect        (format star "edit-indirect .*")
     ;; Common buffers
     :scratch              (format star "[Ss]cratch")
     :help                 (format star "[Hh]elp")
     :man                  (format star "[Mm]an ?.*")
     :eldoc                (format star "[Ee]ldoc")
     :message              (format star "[Mm]essages?")
     :backtrace            (format star "[Bb]acktraces?")
     :warning              (format star "[Ww]arnings?")
     :log                  (format star "[Ll]ogs?")
     :compilation          (format star "[Cc]ompilations?")
     :output               (format star "[Oo]utputs?")
     :command              (format star "[Cc]ommands?")
     :tree-sitter-explorer (format star "[Tt]ree-sitter explorer ?.*")
     :flymake-diagnostics  (format star "[Ff]lymake diagnostics ?.*")))
  "Buffer name patterns.")

(defvar /ns/hidden-buffer-keywords
  '(:starred
    :with-leading-star
    :with-leading-space
    :magit)
  "Buffers that are treated as hidden.")

(defun /ns/buffer-regex (keyword)
  "Get KEYWORD corresponded buffer regex from `pewlib::workspace::buffer-regex-plist'."
  (or (plist-get /ns/buffer-regex-plist keyword)
      (error "Invalid keyword: %S" keyword)))

(defun /ns/map-buffer-regex (keywords &optional concat)
  "Return a list of buffer regex from `pewlib::workspace::buffer-regex-plist' given KEYWORDS.
If CONCAT is non-nil the return value is a single regex string."
  (if concat
      (mapconcat #'/ns/buffer-regex keywords "\\|")
    (mapcar #'/ns/buffer-regex keywords)))

(defun /ns/next-editing-buffer (&optional backwards)
  "Switch to the next editing buffer.
If BACKWARDS is non-nil switch to the previous one."
  (interactive "P")
  (let ((current-buffer (current-buffer))
        (switch-func (if backwards #'previous-buffer #'next-buffer)))
    (funcall switch-func)
    (while (and (not (eq current-buffer (current-buffer)))
                (or (string-match-p (/ns/map-buffer-regex /ns/hidden-buffer-keywords t) (buffer-name))
                    (/extra/dired-buffer-p (buffer-name))))
      (funcall switch-func))))

(defun /ns/previous-editing-buffer ()
  "Switch editing buffer backwards."
  (interactive)
  (/ns/next-editing-buffer :previous))

(defun /ns/close-other-buffers-in-major-mode (mode)
  "Close all other buffers in major MODE but this one."
  (interactive "SMajor mode: ")
  (let ((this-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (if (and (eq mode (buffer-local-value 'major-mode buffer))
               (not (eq this-buffer buffer)))
          (kill-buffer buffer)))))

;;; Windows
(defun /ns/reuse-window-in-buffer ()
  "Make new spawned windows atttempt to reuse current ones.
This is usually useful in some major modes like `grep-mode'."
  (setq-local display-buffer-base-action '((display-buffer-reuse-window
                                            display-buffer-use-some-window))
              display-buffer-alist nil))

(defun /ns/side-window-actions (side slot)
  "Return a list of pre-configured side window actions.
See `display-buffer' for property SIDE, SLOT."
  `((display-buffer-reuse-window display-buffer-in-side-window)
    (reusable-frames . t)
    (inhibit-switch-frame . t)
    (window-height . 0.25)
    (side . ,side)
    (slot . ,slot)))

(defun /ns/side-window-p (window)
  "Return non-nil if WINDOW is a side window."
  (window-parameter window 'window-side))

(defun /ns/side-window-exists-p (&optional side)
  "Return the first side window if there is any, otherwise nil.
If SIDE is given and is one of 'top' 'bottom' 'left' and 'right', check for that
specified side.  If SIDE is nil it means check all sides."
  (window-with-parameter 'window-side side))

(defun /ns/normal-window-p (window)
  "Return t if WINDOW is a normal window."
  (not (/ns/side-window-p window)))

(defun /ns/last-normal-window-p (window)
  "Return t if WINDOW is the last normal window."
  (and (/ns/normal-window-p window)
       (= 1 (length (/ns/list-normal-windows)))))

(defun /ns/list-side-windows ()
  "Return a list of side windows."
  (seq-filter
   (lambda (x) (/ns/side-window-p x))
   (window-list)))

(defun /ns/list-normal-windows ()
  "Return a list of normal (non-side) windows."
  (seq-filter
   (lambda (x) (/ns/normal-window-p x))
   (window-list)))

(defun /ns/pop-window-in-new-tab (arg)
  "Pop the current window into a new tab.
If prefix ARG is presented, pop the window without deleting it from the original
place."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (if (and (null arg) (not (/ns/last-normal-window-p (selected-window))))
        (delete-window))
    (tab-bar-new-tab) ;; Duplicate current layout
    (select-window (car (/ns/list-normal-windows)))
    (switch-to-buffer current-buffer)
    (delete-other-windows)))

(defun /ns/pop-window-in-new-tab-persist ()
  "Pop the current window and keep it in the original tab."
  (interactive)
  (/ns/pop-window-in-new-tab :persist))

(defun /ns/next-window ()
  "Switch to the next window."
  (interactive)
  (other-window 1))

(defun /ns/prev-window ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(defun /ns/close-window ()
  "Close the current window, or the tab if it is the last normal window."
  (interactive)
  (if (/ns/last-normal-window-p (selected-window))
      ;; If there is only one normal window left, close the tab, regardless even
      ;; side windows exist
      (tab-bar-close-tab)
    (delete-window)))

(defun /ns/scroll-other-window-page-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window))

(defun /ns/scroll-other-window-page-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window-down))

(defun /ns/scroll-other-window-line-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window 1))

(defun /ns/scroll-other-window-line-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window -1))

(defun /ns/recenter-other-window ()
  "Scroll other window one page down."
  (interactive)
  (recenter-other-window))

(defun /ns/split-window-below ()
  "Split windows and switch focus to the new one."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun /ns/split-window-right ()
  "Split windows and switch focus to the new one."
  (interactive)
  (split-window-right)
  (other-window 1))

;;; Tabs
(defun /ns/move-tab-next ()
  "Move current tab to the next."
  (interactive)
  (tab-bar-move-tab 1))

(defun /ns/move-tab-prev ()
  "Move current tab to the previous."
  (interactive)
  (tab-bar-move-tab -1))

;;; Frames
(defvar /ns/frame-opacity-adjust-step 5
  "The amount of opacity changed each time.")

(defun /ns/set-frame-opacity (val)
  "Set the opacity of the current frame.
VAL is a number between 0 and 100.  0=transparent/100=opaque"
  (interactive "nFrame Opacity [transparent(0) - opaque(100)]: ")
  (let ((value (cond ((> val 100) 100)
                     ((< val 0) 0)
                     (t val))))
    (message "Set Frame opacity: %d%%" value)
    (set-frame-parameter (selected-frame) 'alpha (cons value value))))

(defun /ns/increase-frame-opacity ()
  "Increase frame opacity."
  (interactive)
  (/ns/set-frame-opacity (+ (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                                /ns/frame-opacity-adjust-step)))

(defun /ns/decrease-frame-opacity ()
  "Decrease frame opacity."
  (interactive)
  (/ns/set-frame-opacity (- (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                                /ns/frame-opacity-adjust-step)))

(defun /ns/pop-window-in-new-frame (arg)
  "Pop the current window into a new frame.
If prefix ARG is presented, pop the window without deleting it from the original
place."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (if (and (null arg) (not (/ns/last-normal-window-p (selected-window))))
        (delete-window))
    (select-frame (make-frame-command))
    (switch-to-buffer current-buffer)
    (delete-other-windows)))

(defun /ns/pop-window-in-new-frame-persist ()
  "Pop the current window and keep it in the original frame."
  (interactive)
  (/ns/pop-window-in-new-frame :persist))

(provide 'pewlib-workspace)
;;; pewlib-workspace.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("/ns/" . "pewlib::workspace::") ("/extra/" . "pewlib::extra::"))
;; End:
