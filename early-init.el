;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

;; Frame setup
;; Remove useless UI elements also avoid errors when some features don't exist in TUI mode
;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Layout-Parameters.html
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(tab-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(horizontal-scroll-bars . 0) default-frame-alist)

;; Manually configure packages
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
