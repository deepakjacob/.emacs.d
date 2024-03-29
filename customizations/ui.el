
;; Font and frame size
;; (set-face-font 'default "Roboto Mono 18")
(setq default-frame-alist
      (append (list '(width  . 72) '(height . 40)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    ;; '(font . "Roboto Mono 18")
                    )))
(set-frame-parameter (selected-frame)
                     'internal-border-width 24)
;;
(set-face-attribute 'default nil :family  "Roboto Mono" :height 160 :weight 'normal :width 'normal)
;; (set-frame-font "-*-SF Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1")
;;(set-face-bold 'bold nil)
(setq ns-use-proxy-icon t)
(setq frame-title-format nil)

;; full path in title bar

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(setq frame-title-format "%b (%f)")
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
(load-theme 'doom-one t)
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

(use-package hungry-delete
  :ensure t
  :bind(
        ("C-c b" . 'hungry-delete-backward)
        ("C-c a" . 'hungry-delete-forward)))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-width 0.50
    which-key-idle-delay 0.10)
  :diminish which-key-mode)

(provide 'init-which-key)

(use-package god-mode
  :ensure t
  :config
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  (add-hook 'post-command-hook 'my-god-mode-update-mode-line)
  (which-key-enable-god-mode-support))

;; You can change the cursor style to visually indicate
;; whether God mode is active as follows:
(defun my-god-mode-update-cursor-type () (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;; mode-line highlight
(defun my-god-mode-update-mode-line ()
  (cond
   (god-local-mode
   ;; (set-face-attribute 'mode-line nil :foreground "#604000" :background "#fff29a")
    ;; (set-face-attribute 'mode-line-inactive nil :foreground "#3f3000" :background "#fff3da")
    )
   (t
    ;; (set-face-attribute 'mode-line nil :foreground "#0a0a0a" :background "#d7d7d7")
    ;; (set-face-attribute 'mode-line-inactive nil	:foreground "#404148" :background "#efefef")
    )))


;; Vertical window divider
(setq window-divider-default-right-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode)
