(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default cursor-type 'bar) ;;makes cursor a line
(blink-cursor-mode 0)

;; window poisition, width and height settings
 (setq default-frame-alist
       '((top . 10) (left . 275) (width . 150) (height . 90)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'doom-one t)

(global-prettify-symbols-mode +1)
(global-linum-mode)
'(linum-format " %3i ")

;; regular and unicode font setting
(set-face-attribute 'default nil :family "Operator Mono")
(set-fontset-font t 'unicode "Fira Mono for Powerline" nil 'prepend)
;; increase font size for better readability
(set-face-attribute 'default nil :height 160)
(set-face-attribute 'fringe nil :background nil)

;; full path in title bar
;; (setq frame-title-format "%b (%f)")
(setq frame-title-format nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
;; no bell
(setq ring-bell-function 'ignore)

;; These settings relate to how emacs interacts with your operating system
;; makes killing/yanking interact with the clipboard
(setq-default
   x-select-enable-clipboard t
   save-interprogram-paste-before-kill t
   apropos-do-all t
   )

;; (require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(use-package fill-column-indicator
  :defer t
  :hook (prog-mode . fci-mode)
  :config
  (setq-default fci-always-use-textual-rule nil)
  (setq-default fci-rule-color "black")
  (setq-default fci-rule-column 100)
  (setq-default fci-rule-use-dashes nil)
  (setq-default fci-rule-width 1)
  )

(use-package highlight-indent-guides
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
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
  (doom-modeline-mode)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-github t)
  (setq doom-modeline-height 25)
  (setq doom-modeline-version nil)
  (setq doom-modeline-bar-width 2)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-python-executable "python")
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  )

;; Turn ^L into nice <hr>
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode t)
)
