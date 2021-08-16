(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq pop-up-windows nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(fset 'yes-or-no-p 'y-or-n-p)
;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
;; No need for ~ files when editing
(setq create-lockfiles nil)
;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)
(tool-bar-mode 0)
(menu-bar-mode -1)
(tooltip-mode  0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(global-hl-line-mode 1)
(electric-pair-mode)
(electric-indent-mode)





;; check whats done to uniquify buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(save-place-mode 1)

;; TODO move to a misc file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
