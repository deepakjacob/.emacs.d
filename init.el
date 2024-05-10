;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)

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
      use-package evil vertico marginalia orderless
    
  ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(defun jd-ui-startup ()
  "Custom startup settings for Emacs."
  (interactive)
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        echo-keystrokes 0.1
        auto-save-default nil
        tab-width 2
        use-dialog-box nil
        frame-height 60
        frame-width 90
        create-lockfiles nil
        indent-tabs-mode nil
        default-tab-width 2
        backup-inhibited t)
  (set-frame-font "Comic Code Ligatures-14" nil t)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode 1)
  (global-hl-line-mode 1)
  (electric-pair-mode 1)
  (electric-indent-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; (global-set-key (kbd "C-c d") 'delete-windows-on)

  (menu-bar-mode -1)
  (tool-bar-mode -1) 
  )
(jd-ui-startup)  ;; Apply UI settings at startup


(use-package evil
  :config
  (evil-mode 1))

  
(use-package vertico
  :config
  (vertico-mode)
  )

; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(add-to-list 'marginalia-prompt-categories '("\\<face\\>" . face))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))



;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(marginalia vertico evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
