;;; early-init.el --- Emacs 27+ pre-initialisation config
;;; Commentary:
;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:
;; Frame setup
;; Remove useless UI elements also avoid errors when some features don't exist in TUI mode
;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Layout-Parameters.html
(dolist (param '((menu-bar-lines . nil)
                 (tool-bar-lines . nil)
                 (tab-bar-lines . nil)
                 (vertical-scroll-bars . nil)
                 (horizontal-scroll-bars . nil)
                 (alpha . (100 . 100))))
  (push param default-frame-alist))

;; Manually configure packages
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
