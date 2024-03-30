;; Frame and font settings
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 90))
(set-frame-font "Fira Code-11" nil t)

;; Remove the menu bar and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode  0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(global-hl-line-mode 1)
(electric-pair-mode)
(electric-indent-mode)
(winum-mode)

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq create-lockfiles nil)
;; Disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq pop-up-windows nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-frame-font "Inconsolata Nerd Font Mono 13" nil t)

;; Load theme
(load-theme 'doom-ir-black t)
