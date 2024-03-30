;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
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
		evil
		evil-collection
  diminish
  smex
  projectile
  rainbow-delimiters
  tagedit
  magit
  counsel
  ivy
  mini-frame
  ivy-posframe

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
  avy
  page-break-lines

  hungry-delete
  lsp-mode
  company
  rust-mode
	lsp-ui
	lsp-ivy
  dap-mode
	
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

;; Set the location for the custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Don't load the custom file, but don't throw an error if it doesn't exist
;;(when (file-exists-p custom-file)
;;  (load custom-file))

;; Define a function to easily load configuration files from the 'customizations' directory
(defun load-custom-config (file)
  (load-file (expand-file-name (concat "customizations/" file) user-emacs-directory)))

;; Load separate config files from the 'customizations' directory
(load-custom-config "startup.el")
(load-custom-config "ui.el")

(load-custom-config "rust-lsp.el")


