;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- this file bootstraps the configuration,

;;; Code:

;; Produce backtraces when errors occur
;; (setq debug-on-error t)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))



;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("melpa stable" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa org" . "http://melpa.org/packages/") t)


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
  ;;clojure-mode

  ;; extra syntax highlighting for clojure
  ;;clojure-mode-extra-font-locking

  ;; integration with a Clojure REPL
  ;; https://github.com/clojure-emacs/cider
  ;;cider

  ;; Enhances M-x to allow easier execution of commands. Provides
  ;; a filterable list of possible commands in the minibuffer
  ;; http://www.emacswiki.org/emacs/Smex
  smex

  projectile

  rainbow-delimiters

  tagedit

  magit

  counsel

  ivy

  swiper

  rjsx-mode

  json-mode

  windmove

  expand-region

  dumb-jump

  neotree

  page-break-lines

  company

  multiple-cursors

  doom-themes

  spacemacs-theme

  highlight-symbol

  use-package

  guide-key

  browse-kill-ring

  move-dup

  winum

  change-inner

  markdown-mode

  smartparens

  focus

  zoom

  fill-column-indicator

  easy-kill

  ;; Emacs Polyglot: an Emacs LSP client that stays out of your way:

  ;;eglot

  doom-modeline

  highlight-indent-guides

  ace-jump-mode

  eslint-fix

  go-mode

  dockerfile-mode

  apropospriate-theme

  typescript-mode

  lsp-mode

  lsp-ui

  lsp-ivy

  lsp-treemacs
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
(add-to-list 'load-path  "~/.emacs.d/vendor")
;; (add-to-list 'load-path  "~/.fonts/fira-code.el")
;; (add-to-list 'load-path  "~/.fonts/fira-code-data.el")

(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))
;;;;
;; Customization
;;;
;;;;
;;;
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; For editing lisps
(load "elisp-editing.el")

;;; enhancements to buffers eg, flycheck mode
(load "enhancements.el")

(load "window.el")

(require 'lsp-mode)
(add-hook 'prog-mode-hook #'lsp)
;;(add-hook 'prog-mode-hook #'fira-code-mode)

(add-hook 'before-save-hook #'lsp-format-buffer)
(add-hook 'before-save-hook #'lsp-organize-imports)

(setq ns-function-modifier 'super)  ; make Fn key do Hyper


;;; init ends here
