;;; -*- coding: utf-8; mode: lisp-data; -*-

(;; General todo task
 ("t" "New todo item in agenda" entry
  (file+olp+datetree "agenda.org")
  "* TODO %?\n"
  :empty-lines 1
  :tree-type month)

 ;; Note taking
 ("n" "New note" plain
  (function (lambda ()
              (let* ((file (read-file-name "Start with file: "
                                             (expand-file-name (concat "notes/"
                                                                       (format-time-string "%Y%m%d%H%M-"))
                                                               org-directory))))
                (unless (string-match-p "\\.org$" file)
                  (setq file (concat file ".org")))
                ;; https://stackoverflow.com/questions/56274067/how-to-use-the-function-option-of-org-capture-correctly
                (set-buffer (org-capture-target-buffer file))
                ;; Pass infomation to the capture buffer via `kill-ring'
                (if (org-collect-keywords '("title" "date"))
                    (kill-new "resumed-capture")
                  ;; Title of this capture
                  (kill-new (string-trim (file-name-base file) "[0-9]\\{12\\}-")))
                ;; Finally move cursor to the end
                (goto-char (point-max)))))
  "%(if (string-equal \"resumed-capture\" (car kill-ring))
        (ignore (pop kill-ring))
      (with-temp-buffer
        (insert \"#+title: \"
                (pop kill-ring)
                \"\n\"
                \"#+date: \")
        (org-time-stamp '(16))
        (buffer-string)))

%?"
  :kill-buffer t
  ;; Show all content if amending to the existing note file
  :unnarrowed t))
