;;; elpa-minibuffer-ivy.el --- ivy and complementary -*- lexical-binding: t; -*-
;;; Commentary:

;; NOTE: The `counsel' contains ivy, counsel and swipper.
;; See: https://oremacs.com/swiper/#installing-from-emacs-package-manager

;;; Code:

(use-package counsel
  :demand t
  :bind ( ("C-s" . swiper)
          ("C-x d" . counsel-dired)
          ("C-x f" . counsel-find-file)
          ("C-c r" . ivy-resume)
          ("C-c f" . counsel-git)
          ("C-c h" . counsel-git-grep)
          ("C-c g" . counsel-ag)
          :map minibuffer-local-map
          ("C-r" . counsel-minibuffer-history) )

  :custom
  (ivy-use-virtual-buffers t)
  ;; enable this if you want `swiper' to use it
  ;;(search-default-mode #'char-fold-to-regexp)
  (ivy-count-format "(%d/%d) ")
  (ivy-on-del-error-function #'ignore)
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  ;; Force minimal number of chars required for all searches
  (ivy-more-chars-alist '((t . 2)))

  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (mapcar (lambda (name) (add-to-list 'ivy-ignore-buffers name t)) (pewlib::map-buffer-regex pewlib::hidden-buffer-keywords)))

(use-package ivy-rich
  :requires ivy
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

(use-package ivy-prescient
  :requires ivy
  :custom
  (ivy-prescient-enable-filtering nil)

  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package counsel-projectile
  :after (ivy projectile)
  :custom
  (projectile-completion-system 'ivy)

  :config
  (counsel-projectile-mode 1))

(use-package lsp-ivy
  :after (ivy lsp)
  :commands lsp-ivy-workspace-symbol)

(use-package which-key
  :custom
  (which-key-popup-type 'side-window)
  (which-key-show-early-on-C-h nil)
  (which-key-idle-delay 1.0)

  :config
  (which-key-mode 1)
  ;; Minibuffer usually causes display problems
  ;;(which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom))

(provide 'elpa-minibuffer-ivy)
;;; elpa-minibuffer-ivy.el ends here
