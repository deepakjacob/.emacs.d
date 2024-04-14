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
      smart-mode-line
      winum
      which-key

      lsp-mode
      lsp-ui
      embark
      embark-consult
      marginalia
      vertico
      rust-mode
      corfu
      
      orderless
      cape
      
      
  ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun jd-ui-startup ()
  "Custom startup settings for Emacs."
  (interactive)
  (setq inhibit-startup-screen t)       ;; Inhibit startup screen
  (setq inhibit-startup-echo-area-message t)
  (menu-bar-mode -1)              ;; Disable menu bar
  (tool-bar-mode -1)              ;; Disable toolbar
  (setq-default echo-keystrokes 0.1)            ;; Startup echo area message
  (setq-default auto-save-default nil)          ;; Disable auto-save
  (setq-default tab-width 2)                    ;; Set tab width to 2
  (setq-default use-dialog-box nil)             ;; Disable pop-up dialog boxes
  (setq-default frame-height 60)                ;; Set frame height to 60
  (setq-default frame-width 90)                 ;; Set frame width to 90
  (set-frame-font "Inconsolata Nerd Font Mono" nil t) ;; Set font to Inconsolata Nerd Font Mono
  (tooltip-mode -1)                             ;; Disable tooltips
  (scroll-bar-mode -1)                          ;; Disable scrollbar
  (show-paren-mode 1)                           ;; Show matching parentheses
  (global-hl-line-mode 1)                       ;; Highlight current line
  (electric-pair-mode 1)                        ;; Enable automatic insertion of matching pairs
  (electric-indent-mode 1)                      ;; Enable electric indentation
  (winum-mode 1)                                ;; Use winum mode for window numbering
  (setq-default create-lockfiles nil)           ;; Disable lock files
  (setq-default indent-tabs-mode nil)           ;; Use spaces for indentation
  (setq-default default-tab-width 2)            ;; Set default tab width to 2
  (fset 'yes-or-no-p 'y-or-n-p)                ;; Change yes-or-no-p to y or n
  )
;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))
(jd-ui-startup)  ;; Call jd-ui-startup to apply the settings
(setq sml/no-confirm-load-theme t)

(sml/setup)
(setq sml/theme 'dark)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))
;;(setq custom-file null-device) ;; Set custom-file to null device

;; Configure embark for enhanced documentation lookup
(require 'embark)
(require 'embark-consult)
(add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)

;; Enable marginalia for annotation in minibuffer
(require 'marginalia)
(marginalia-mode)

;; Configure vertico for better completion
(require 'vertico)
(vertico-mode)
;; Optional cape package.
;; See the Cape README for more tweaks!


(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))
  ;; Enable auto completion and configure quitting


  
(setq corfu-auto t
      corfu-quit-no-match 'separator) ;; or t

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
      
;; Optional cape package.
;; See the Cape README for more tweaks!
(use-package cape)

(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!

  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    ;; Optionally configure the cape-capf-buster.
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

  :hook (
  (rust-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)


(setq custom-file (expand-file-name "emacs-custom.el" temporary-file-directory))
;;(load custom-file 'no-error)
