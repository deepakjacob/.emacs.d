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
    ;; use C-x-o to navigate windows within a frame
    windmove


    ;; expand-region
    expand-region

    ;;dump-jump
    dumb-jump

    ;;neotree
    neotree

    page-break-lines

    smooth-scrolling

    company

    multiple-cursors

    js2-refactor

    use-package

    doom-themes

    ;; highlight symbols
    highlight-symbol

    ;; easy loading of packages
    use-package

    ;; display keyboard shortcuts
    guide-key

    ;; see kill ring contents
    browse-kill-ring

    ;; duplicate region above / below
    move-dup

    ;; remove clutter while displaying minor modes
    diminish

    ;; window switching
    winum

    jbeans-theme

    ;;elixir
    elixir-mode

    ;; for elixir / phoenix / mix
    alchemist

    ;; vim style tet deletion within '',"", ``, [], {}, () etc
    change-inner

    ;; markdown mode
    markdown-mode

    ;; telephone-line
    telephone-line

    ;; smartparens
    smartparens

    go-mode

    go-autocomplete

    go-guru

    focus

    zoom

    fill-column-indicator

    ))

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
;;(load "setup-clojure.el")

(load "setup-js.el")

;; code formatting
(load "prettier-js.el")

(load "enhancements.el")

(global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))


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
    (global-text-scale-mode 1))
  (global-set-key (kbd "M-0")
                  '(lambda () (interactive)
                     (global-text-scale-adjust (- text-scale-mode-amount))
                     (global-text-scale-mode -1)))
  (global-set-key (kbd "M-+")
                  '(lambda () (interactive) (global-text-scale-adjust 1)))
  (global-set-key (kbd "M-_")
                  '(lambda () (interactive) (global-text-scale-adjust -1)))

(require 'elixir-mode)
(require 'smartparens-config)
(add-hook 'elixir-mode-hook #'smartparens-mode)
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
         :when '(("SPC" "RET"))
         :actions '(insert navigate))
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(sp-ruby-def-post-handler)
         :actions '(insert navigate)))

(require 'telephone-line)

(setq telephone-line-lhs
      '((accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))))
(setq telephone-line-height 18)

(telephone-line-mode 1)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "gray0" :foreground "gray100"))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:background "gray0" :foreground "gray100"))))
 '(telephone-line-accent-active ((t (:inherit mode-line :background "orange3" :foreground "white"))))
 '(telephone-line-accent-inactive ((t (:inherit mode-line-inactive :background "saddle brown" :foreground "white"))))
 '(telephone-line-evil ((t (:inherit mode-line :background "gray0" :foreground "white" :weight bold))))
 '(vc-state-base ((t (:background "gray0")))))
