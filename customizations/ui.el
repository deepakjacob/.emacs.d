;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)
;; Turn of the tool bar at the top of the each frame because it's distracting
(tool-bar-mode -1)

(setq-default cursor-type 'bar) ;;makes cursor a line
;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;;default starting emacs size
(add-to-list 'default-frame-alist '(height . 90))
(add-to-list 'default-frame-alist '(width . 100))

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'doom-one t)
;;(zerodark-setup-modeline-format)

;; Show line numbers
(global-linum-mode)

;; Chnage the font to Operator Mono
(set-face-attribute 'default nil :font "Fira Mono for Powerline") ;; font
;; increase font size for better readability
(set-face-attribute 'default nil :height 120)
(set-face-attribute 'fringe nil :background nil)

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      ;; x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t


      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      ;; mouse-yank-at-point t
      )

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-auto-character-face-perc 10)


(global-prettify-symbols-mode +1)

;; full path in title bar
(setq-default frame-title-format '("")) ;;frame title
;;(setq-default frame-title-format "%b (%f)")

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


(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; (setq frame-title-format
;;      '((:eval (if (buffer-file-name)
;;                   (abbreviate-file-name (buffer-file-name))
;;                 "%b"))))

(custom-set-variables
 '(doom-neotree-folder-size 1.0)
 '(fci-always-use-textual-rule nil)
 '(fci-rule-color "light green")
 '(fci-rule-use-dashes nil)
 '(fci-rule-width 1)
 '(highlight-indent-guides-character 124)
 '(jdee-db-active-breakpoint-face-colors (cons "#000000" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#000000" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#000000" "#525254"))
 '(linum-format " %3i ")
 '(neo-theme (quote ascii))
 '(neo-window-fixed-size nil)
 '(neo-window-position (quote right))
 '(neo-window-width 65)
 )


;; (custom-set-variables '(zoom-mode t))


