(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default cursor-type 'bar) ;;makes cursor a line
(blink-cursor-mode 0)

;;default starting emacs size
(add-to-list 'default-frame-alist '(height . 90))
(add-to-list 'default-frame-alist '(width . 150))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'doom-one)

;; Show line numbers
(global-linum-mode)
'(linum-format " %3i ")

(set-face-attribute 'default nil :family "Operator Mono") ;; font
(set-fontset-font t 'unicode "Fira Mono for Powerline" nil 'prepend)

;; increase font size for better readability
(set-face-attribute 'default nil :height 130)
(set-face-attribute 'fringe nil :background nil)

;; These settings relate to how emacs interacts with your operating system
(setq-default ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      )

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq-default highlight-indent-guides-method 'character)
(setq-default highlight-indent-guides-auto-character-face-perc 10)
(setq-default highlight-indent-guides-responsive 'top)
(setq-default highlight-indent-guides-character 124)

(global-prettify-symbols-mode +1)

;; full path in title bar
;; (setq-default frame-title-format '("")) ;;frame title
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)

;; load the all-the-icons prior to loading neotree
(require 'all-the-icons)
(require 'neotree)
(define-key global-map (kbd "C-c n") 'neotree-toggle)
(define-key global-map (kbd "C-c i") 'neotree-dir)
(setq-default neo-theme (quote ascii))
(setq-default neo-window-fixed-size nil)
(setq-default neo-window-position 'right)
(setq-default neo-window-width 35)
(setq-default doom-neotree-folder-size 1.0)


;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(if (display-graphic-p)
      ;;smooth scrolling in gui
      (use-package smooth-scrolling
        :ensure t
        :init (smooth-scrolling-mode 1)
        :diminish smooth-scrolling-mode
        :config
        (setq smooth-scroll-margin 1)
        (smooth-scrolling-mode 1)))

;; Turn ^L into nice <hr>
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode t)
)

(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq-default fci-always-use-textual-rule nil)
(setq-default fci-rule-color "deep orange")
(setq-default fci-rule-column 100)
(setq-default fci-rule-use-dashes nil)
(setq-default fci-rule-width 1)


(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; (setq frame-title-format
;;      '((:eval (if (buffer-file-name)
;;                   (abbreviate-file-name (buffer-file-name))
;;                 "%b"))))


;; (custom-set-variables '(zoom-mode t))


(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(require 'doom-modeline)
(doom-modeline-init)
;; How tall the mode-line should be (only respected in GUI Emacs).
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be (only respected in GUI Emacs).
(setq doom-modeline-bar-width 2)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are expereicing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; What executable of Python will be used (if nil nothing will be showed).
(setq doom-modeline-python-executable "python")

;; Whether show `all-the-icons' or not (if nil nothing will be showed).
(setq doom-modeline-icon t)

;; Whether show the icon for major mode. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display minor modes or not. Non-nil to display in mode-line.
(setq doom-modeline-minor-modes nil)

;; Whether display perspective name or not. Non-nil to display in mode-line.
(setq doom-modeline-persp-name t)

;; Whether display `lsp' state or not. Non-nil to display in mode-line.
(setq doom-modeline-lsp t)

;; Whether display github notifications or not. Requires `ghub` package.
(setq doom-modeline-github t)

;; The interval of checking github.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display environment version or not.
(setq doom-modeline-version nil)
