
;; Font and frame size
(set-face-font 'default "Roboto Mono Light 14")
(setq default-frame-alist
      (append (list '(width  . 72) '(height . 40)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    '(font . "Roboto Mono Light 14"))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 24)
;;
;;(set-face-attribute 'default nil :family  "Roboto Mono" :height 130 :weight 'medium :width 'normal)
;; (set-frame-font "-*-SF Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1")
;;(set-face-bold 'bold nil)
(setq ns-use-proxy-icon t)
(setq frame-title-format nil)

;; full path in title bar

;;(set-frame-parameter (selected-frame) 'alpha '(98 . 97))
;; (setq frame-title-format "%b (%f)")
;; (setq frame-title-format nil)
;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)
;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)
;; No ugly button for checkboxes
(setq widget-image-enable nil)
;; Line cursor and no blink
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)
;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)
;; No Tooltips
(tooltip-mode 0)
;; Paren mode is part of the theme
(show-paren-mode t)
;; No fringe but nice glyphs for truncated and wrapped lines
(fringe-mode '(0 . 0))
(set-face-attribute 'fringe nil :background nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(global-prettify-symbols-mode +1)
;;(global-linum-mode)
;;'(linum-format " %3i ")
(normal-erase-is-backspace-mode +1)
(load-theme 'spacemacs-light t)
;; These settings relate to how emacs interacts with your operating system
;; makes killing/yanking interact with the clipboard
(setq-default
   x-select-enable-clipboard t
   save-interprogram-paste-before-kill t
   apropos-do-all t
   )


;; Following should not be enabled due to custom header
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-icon nil))

;; Turn ^L into nice <hr>
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode t))



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


              
;; Vertical window divider
(setq window-divider-default-right-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode)



