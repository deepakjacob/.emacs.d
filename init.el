
(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(setq gc-cons-threshold (* 100 1024 1024))

;; Define package repositories
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


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
  '(
  smex
  projectile
  rainbow-delimiters
  tagedit
  magit
  counsel
  ivy
  swiper
  windmove
  expand-region

  neotree
  page-break-lines
  multiple-cursors
  doom-themes
  spacemacs-theme
  highlight-symbol
  use-package
  which-key
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
  doom-modeline
  highlight-indent-guides
  ace-jump-mode
  page-break-lines

  go-mode
  lsp-mode
  lsp-ui
  flycheck
  lsp-ivy
  dap-mode
  company

  god-mode
  hungry-delete
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


;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")
(load "startup.el")
(load "ui.el")
(load "shortcuts.el")
(load "lspmode.el")


;; TODO find a good place to place this
;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
