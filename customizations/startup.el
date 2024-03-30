;; Frame and font settings
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 90))
(set-frame-font "Fira Code-11" nil t)

;; Remove the menu bar and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Load theme
(load-theme 'modus-operandi t)
