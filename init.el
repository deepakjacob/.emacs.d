;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Ensure all packages are available
(unless package-archive-contents
  (package-refresh-contents))

;; Straight.el bootstrap code
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Define packages to be installed
(defvar my-packages
  '(doom-themes use-package evil evil-collection smart-mode-line winum which-key dirvish
    lsp-mode lsp-ui embark embark-consult marginalia vertico rust-mode corfu
    orderless cape kind-icon svg-lib all-the-icons))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(defun jd-ui-startup ()
  "Custom startup settings for Emacs."
  (interactive)
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        menu-bar-mode -1
        tool-bar-mode -1
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
  (winum-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (load-theme 'doom-ir-black t))
(jd-ui-startup)  ;; Apply UI settings at startup

(use-package which-key
  :config
  (which-key-mode))

(use-package evil
  :config
  (evil-mode 1))

(use-package embark
  :requires embark-consult
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package vertico
  :config
  (vertico-mode))

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

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; Corfu is used
  :commands lsp
  :hook ((rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  :bind (:map dirvish-mode-map
              ("a" . dirvish-quick-access)))

;; Vertical window divider settings
(setq window-divider-default-right-width 1
      window-divider-default-places 'right-only)
(window-divider-mode)


