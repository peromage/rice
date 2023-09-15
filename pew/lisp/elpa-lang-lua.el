;;; elpa-lang-lua.el --- Lua mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Support for Lua development.

;;; Code:
(use-package lua-mode
  :config
  (pewcfg
    :setq
    (lua-indent-level 4)))

(provide 'elpa-lang-lua)
;;; elpa-lang-lua.el ends here
