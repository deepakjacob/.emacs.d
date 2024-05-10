(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(use-package evil vertico marginalia orderless))

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
