;: Define package repositories
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
    ;; clojure-mode

    ;; extra syntax highlighting for clojure
    ;; clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    ;; cider

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

    ;; a nice theme
    zerodark-theme

    ;; another theme
    spacemacs-theme

    ;; ace-window
    ;; use C-x-o to navigate windows within a frame
    windmove

    ;; expand selection to next scope
    expand-region

    ;;jump to symbol / definitions across files in a project in a totally dumb manner :)
    dumb-jump

    ;; a file tree to the left of the editor
    neotree

    ;;  mode line config
    spaceline

    ;; nice lines
    page-break-lines

    ;;smooth-scrolling
    company

    ;; commenting as this is not very helpful in day to day coding effort
    ;; company-tern

    ;; edit usiing multiple cursor
    multiple-cursors

    ;; refactor javascript
    js2-refactor

    
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

    ;; for elixir / phoenix / mix
    alchemist

    ;; vim style tet deletion within '',"", ``, [], {}, () etc
    change-inner
    
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
;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

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
  (global-set-key (kbd "M-_")
                  '(lambda () (interactive) (global-text-scale-adjust -1)))


(require 'spaceline-config)
(spaceline-emacs-theme)
(spaceline-info-mode)



(require 'diminish)
(eval-after-load "projectile"
  '(diminish 'projectile-mode))

(eval-after-load "yas-minor-mode"
  '(diminish 'yas-minor-mode))

(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode))

(eval-after-load 'flycheck
  '(diminish 'flycheck-mode))
(diminish 'abbrev-mode)
(eval-after-load 'auto-revert-mode
  '(diminish 'auto-revert-mode))

(eval-after-load 'company
  '(diminish 'company-mode))

(eval-after-load 'paredit
  '(diminish 'paredit-mode))

(eval-after-load 'ivy-mode
  '(diminish 'ivy-mode))

(eval-after-load 'js2-refactor-mode
  '(diminish 'js2-refactor-mode))


(eval-after-load 'tern-mode
  '(diminish 'tern-mode))

(eval-after-load 'global-eldoc-mode
  '(diminish 'global-eldoc-mode))


(eval-after-load 'auto-revert-mode
  '(diminish 'auto-revert-mode))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-banner-face ((t (:foreground "lightblue" :weight normal))))
 '(powerline-active1 ((t (:inherit mode-line :background "firebrick"))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray17" :foreground "white"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "chocolate3"))))
 '(powerline-inactive2 ((t (:inherit mode-line :background "gray17")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "230302a8dba6a7d46cc37709795427e229e67d5e6817db4f90e370c67766cdb6" "42b8102c1234a9f680722953161c1127cc59ec68ad8d5c710af60d68c3b6e6ef" "73e35ffa5ca98b57a9923954f296c3854ce6d8736b31fdbdda3d27502d4b4d69" "d0404bd38534a00ee72a4f887a987d6bff87f4cf8d8f85149e32849b262465a5" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "77bddca0879cb3b0ecdf071d9635c818827c57d69164291cb27268ae324efa84" "0a3a41085c19d8121ed0ad3eb658a475ccb948a70a83604641ee7d4c3575a4d5" default)))
 '(fci-rule-color "#5c5e5e")
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#41728e"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#5a5b5a"))
 '(linum-format " %3i ")
 '(neo-window-position (quote right))
 '(neo-window-width 45)
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (dracula-theme change-inner zerodark-theme winum use-package ujelly-theme tagedit spacemacs-theme spaceline smex rjsx-mode rainbow-delimiters projectile paredit page-break-lines neotree move-dup json-mode js2-refactor jbeans-theme highlight-symbol guide-key expand-region exec-path-from-shell dumb-jump doom-themes dockerfile-mode dired-details+ dired+ diminish counsel browse-kill-ring alchemist)))
 '(powerline-gui-use-vcs-glyph t)
 '(vc-annotate-background "#0d0d0d")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b5bd68")
    (cons 40 "#c8c06c")
    (cons 60 "#dcc370")
    (cons 80 "#f0c674")
    (cons 100 "#eab56d")
    (cons 120 "#e3a366")
    (cons 140 "#de935f")
    (cons 160 "#d79e84")
    (cons 180 "#d0a9a9")
    (cons 200 "#c9b4cf")
    (cons 220 "#ca9aac")
    (cons 240 "#cb8089")
    (cons 260 "#cc6666")
    (cons 280 "#af6363")
    (cons 300 "#936060")
    (cons 320 "#765d5d")
    (cons 340 "#5c5e5e")
    (cons 360 "#5c5e5e")))
 '(vc-annotate-very-old-color nil))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
