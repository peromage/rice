;;; elpa-ivy.el --- Completion framework -*- lexical-binding: t -*-
;;; Commentary:

;; Ivy uses minibuffers to show completions.
;; Unlike helm, Ivy is faster and minimalist.  Also, easier to configure.

;;; Code:
;;;; Ivy core package

;; Contains ivy, counsel and swipper
;; See: https://oremacs.com/swiper/#installing-from-emacs-package-manager
(use-package counsel
  :demand t
  :diminish (ivy-mode counsel-mode)
  :bind (("C-s" . swiper)
         ("C-c r" . ivy-resume)
         ("C-x d" . counsel-dired)
         ("C-x f" . counsel-find-file)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-git)
         ("C-c h" . counsel-git-grep)
         ("C-c g" . counsel-ag)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :custom
  (ivy-use-virtual-buffers t)
  ;; enable this if you want `swiper' to use it
  ;(search-default-mode #'char-fold-to-regexp)
  (ivy-count-format "(%d/%d) ")
  (ivy-on-del-error-function #'ignore)
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  ;; Force minimal number of chars required for all searches
  (ivy-more-chars-alist '((t . 2)))
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (mapcar (lambda (name) (add-to-list 'ivy-ignore-buffers name t)) pew/special-buffers))

;;;; Ivy improvement

;; Make Ivy show more information
(use-package ivy-rich
  :custom
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1)
  (ivy-rich-set-columns
   'ivy-switch-buffer
   '((ivy-rich-switch-buffer-indicators
      (:width 4 :face error :align left))
     (ivy-switch-buffer-transformer
      (:width 0.35))
     (ivy-rich-switch-buffer-major-mode
      (:width 16 :face warning :align left))
     (ivy-rich-switch-buffer-size
      (:width 7 :align right))
     (ivy-rich-switch-buffer-project
      (:width 0.18 :face success :align right))
     (ivy-rich-switch-buffer-path
      (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path
                           x
                           (ivy-rich-minibuffer-width 0.3))) :align left)))))

;; Help sort candidates and also keep the most recent history on the top
(use-package ivy-prescient
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(provide 'elpa-ivy)
;;; elpa-ivy.el ends here
