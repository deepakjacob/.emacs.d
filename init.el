;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
;; (defvar ido-cur-item nil)
;; (defvar ido-default-item nil)
;; (defvar ido-cur-list nil)
;; (defvar predicate nil)
;; (defvar inherit-input-method nil)

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ;; ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    counsel

    ;; for general completion
    ivy

    ;; for navigtion
    swiper

    ;; for react
    rjsx-mode

    ;; for json
    json-mode

    ;; zerodark theme
    zerodark-theme

    ;; spacemacs
    spacemacs-theme

    ;; ace-window
    ace-window

    ;; expand-region
    expand-region

    ;;dump-jump
    dumb-jump

    ;;neotree
    neotree
    
    spaceline

    page-break-lines

    smooth-scrolling

    company

    multiple-cursors

    js2-refactor
    
    doom-themes
    
    highlight-symbol

    use-package))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")

(load "setup-js.el")

(global-set-key "\C-cy" '(lambda ()
                                 (interactive)
                                 (popup-menu 'yank-menu)))


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
            (next-win-buffer (window-buffer (next-window)))
            (this-win-edges (window-edges (selected-window)))
            (next-win-edges (window-edges (next-window)))
            (this-win-2nd (not (and (<= (car this-win-edges)
                                        (car next-win-edges))
                                    (<= (cadr this-win-edges)
                                        (cadr next-win-edges)))))
            (splitter
             (if (= (car this-win-edges)
                    (car (window-edges (next-window))))
                 'split-window-horizontally
               'split-window-vertically)))
       (delete-other-windows)
       (let ((first-win (selected-window)))
         (funcall splitter)
         (if this-win-2nd (other-window 1))
         (set-window-buffer (selected-window) this-win-buffer)
         (set-window-buffer (next-window) next-win-buffer)
         (select-window first-win)
         (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

(define-globalized-minor-mode
    global-text-scale-mode
    text-scale-mode
    (lambda () (text-scale-mode 1)))

  (defun global-text-scale-adjust (inc) (interactive)
    (text-scale-set 1)
    (kill-local-variable 'text-scale-mode-amount)
    (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
    (global-text-scale-mode 1)
  )
  (global-set-key (kbd "M-0")
                  '(lambda () (interactive)
                     (global-text-scale-adjust (- text-scale-mode-amount))
                     (global-text-scale-mode -1)))
  (global-set-key (kbd "M-+")
                  '(lambda () (interactive) (global-text-scale-adjust 1)))
  (global-set-key (kbd "M--")
                  '(lambda () (interactive) (global-text-scale-adjust -1)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(beacon-color "#d54e53")
 '(custom-enabled-themes (quote (doom-one)))
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "31e64af34ba56d5a3e85e4bebefe2fb8d9d431d4244c6e6d95369a643786a40e" "4b207752aa69c0b182c6c3b8e810bbf3afa429ff06f274c8ca52f8df7623eb60" "60668f4b17b8b8780d50976c0788abed190353d21d3371b8f244dd44c103b0ea" "d2c61aa11872e2977a07969f92630a49e30975220a079cd39bec361b773b4eb3" "4a7abcca7cfa2ccdf4d7804f1162dd0353ce766b1277e8ee2ac7ee27bfbb408f" "759416a7a5f5cb6b8cb26e6db2cf70026aa2324083a888015ee2cad0320f7f19" "230302a8dba6a7d46cc37709795427e229e67d5e6817db4f90e370c67766cdb6" "10e3d04d524c42b71496e6c2e770c8e18b153fcfcc838947094dad8e5aa02cef" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "ab928e0eedcdf9323f9589b7d761c6f5c746b5621fb0224837808f18c18981e8" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d507c9e58cb0eb8508e15c8fedc2d4e0b119123fab0546c5fd30cadd3705ac86" "44c566df0e1dfddc60621711155b1be4665dd3520b290cb354f8270ca57f8788" "365d9553de0e0d658af60cff7b8f891ca185a2d7ba3fc6d29aadba69f5194c7f" "5900bec889f57284356b8216a68580bfa6ece73a6767dfd60196e56d050619bc" "0c387e27a3dd040b33c6711ff92e13bd952369a788eee97e4e4ea2335ac5528f" "9f569b5e066dd6ca90b3578ff46659bc09a8764e81adf6265626d7dc0fac2a64" default)))
 '(fci-rule-color "#5B6268")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (info+ spaceline flycheck-elixir flycheck-clojure eldoc-eval zerodark-theme vim-empty-lines-mode use-package tagedit spacemacs-theme smooth-scrolling smex skewer-mode rjsx-mode rainbow-delimiters projectile paredit page-break-lines ov neotree nav-flash multi-term json-mode js2-refactor indent-guide highlight-symbol github-theme github-modern-theme expand-region exec-path-from-shell dumb-jump doom-themes dockerfile-mode dash-functional counsel company color-theme-sanityinc-tomorrow color-theme-github clojure-mode-extra-font-locking cider bubbleberry-theme badger-theme auto-complete all-the-icons-ivy all-the-icons-gnus all-the-icons-dired ace-window)))
 '(powerline-default-separator (quote slant))
 '(powerline-gui-use-vcs-glyph t)
 '(powerline-image-apple-rgb t)
 '(powerline-utf-8-separator-right 57520)
 '(vc-annotate-background "#1B2229")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active0 ((t (:inherit default :stipple nil :family "Operator Mono"))))
 '(powerline-active1 ((t (:inherit mode-line-emphasis :background "firebrick" :foreground "gray100"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "dark orange"))))
 '(tool-bar ((t (:background "gray100" :box (:line-width 1 :style released-button))))))

(require 'spaceline-config)
(spaceline-emacs-theme)
(spaceline-info-mode)

;;(setq default-frame-alist '((undecorated . t)))
