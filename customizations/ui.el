;;; ui.el --- various ui enhancements
;;; Commentary:

;;; Code:

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default cursor-type 'bar) ;;makes cursor a line
(blink-cursor-mode 0)

;; window poisition, width and height settings
 (setq default-frame-alist
       '((top . 10) (left . 275) (width . 115) (height . 90)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'doom-one t)
;; (load-theme 'seti)
;; (zerodark-setup-modeline-format)

(global-prettify-symbols-mode +1)
;;(global-linum-mode)
;;'(linum-format " %3i ")

(normal-erase-is-backspace-mode +1)

;; regular and unicode font setting

;;
(set-face-attribute 'default nil :family  "Fira Code" :height 150 :weight 'medium :width 'normal)

;; (set-frame-font "-*-SF Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1")

(set-face-attribute 'fringe nil :background nil)
(set-face-bold 'bold nil)
(setq ns-use-proxy-icon t)
(setq frame-title-format nil)

;; full path in title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(set-frame-parameter (selected-frame) 'alpha '(98 . 97))
;; (setq frame-title-format "%b (%f)")
;; (setq frame-title-format nil)
;; new changes end

;; assuming you are using a dark theme
;; no bell
(setq ring-bell-function 'ignore)

;; These settings relate to how emacs interacts with your operating system
;; makes killing/yanking interact with the clipboard
(setq-default
   x-select-enable-clipboard t
   save-interprogram-paste-before-kill t
   apropos-do-all t
   )

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(use-package fill-column-indicator
  :defer t
  ;; :hook (prog-mode . fci-mode)
  :config
  (setq-default fci-always-use-textual-rule nil)
  (setq-default fci-rule-color "black")
  (setq-default fci-rule-column 80)
  (setq-default fci-rule-use-dashes nil)
  (setq-default fci-rule-width 1)
  )

(use-package highlight-indent-guides
  :defer t
  ;; :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-auto-enabled t)
  (setq-default highlight-indent-guides-method 'character)
  (setq-default highlight-indent-guides-auto-character-face-perc 10)
  (setq-default highlight-indent-guides-responsive 'top)
  (setq-default highlight-indent-guides-character 124)
  )

(use-package neotree
  :defer t
  :bind
  (("C-c n" . neotree-toggle)
  ("C-c i" . neotree-dir)
  ("C-c f" . neotree-find))
  :config
  (setq-default neo-theme (quote ascii))
  (setq-default neo-window-fixed-size nil)
  (setq-default neo-window-position 'right)
  (setq-default neo-window-width 45)
  (setq-default doom-neotree-folder-size 1.0)
  )

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-icon nil)


  )

;; Turn ^L into nice <hr>
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode t)
)


(add-hook 'rjsx-mode-hook 'prettify-symbols-mode)
;;; ui.el ends here
