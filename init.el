(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/") t)
	     

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(use-package evil cape lsp-mode lsp-ui corfu vertico marginalia orderless embark consult))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun jd-ui-startup ()
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
  (menu-bar-mode -1)
  (tool-bar-mode -1))
(jd-ui-startup)

(use-package evil
  :config
  (evil-mode 1))

(use-package vertico
  :config
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (add-to-list 'marginalia-prompt-categories '("\\<face\\>" . face)))

  
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :requires embark-consult
  :requires vertico-multiform-mode
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))  

(setq embark-indicators
      '(embark-minimal-indicator  ; default is embark-mixed-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(use-package vertico-multiform
  :after vertico
  :config
  (vertico-multiform-mode))


(add-to-list 'vertico-multiform-categories '(embark-keybinding grid))


(use-package corfu
  :custom
  (corfu-auto t)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package cape)

(use-package lsp-mode
  :requires cape
  :config
  (setq lsp-headerline-breadcrumb-enable nil) 
  :custom
  (lsp-completion-provider :none) ;; Corfu is used
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/orderless-dispatch-flex-first (_pattern index _total) (and (eq index 0) 'orderless-flex))
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(orderless))
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))
  :hook ((rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp
  )

(use-package lsp-ui
  :commands lsp-ui-mode)


(add-to-list 'load-path "~/.emacs.d/customizations")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
 '(package-selected-packages '(rust-mode gruvbox-theme embark marginalia vertico evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
