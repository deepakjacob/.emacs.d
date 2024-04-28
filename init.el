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
  (load-theme 'doom-ir-black t)
  (global-set-key (kbd "C-c d") 'delete-windows-on)

  (menu-bar-mode -1)
  (tool-bar-mode -1) 
  )
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

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir "/tmp/svg-lib/cache/") ; Change cache dir

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache)))
  )

(use-package all-the-icons
  :if (display-graphic-p))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind (
         ("C-c f" . dirvish-fd)
	 :map dirvish-mode-map
              ("a" . dirvish-quick-access)
              ("f"   . dirvish-file-info-menu)
              ("y"   . dirvish-yank-menu)
              ("N"   . dirvish-narrow)
              ("^"   . dirvish-history-last)
              ("h"   . dirvish-history-jump) ; remapped `describe-mode'
              ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
              ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
              ("TAB" . dirvish-subtree-toggle)
              ("M-f" . dirvish-history-go-forward)
              ("M-b" . dirvish-history-go-backward)
              ("M-l" . dirvish-ls-switches-menu)
              ("M-m" . dirvish-mark-menu)
              ("M-t" . dirvish-layout-toggle)
              ("M-s" . dirvish-setup-menu)
              ("M-e" . dirvish-emerge-menu)
              ("M-j" . dirvish-fd-jump)))
;; Vertical window divider settings
(setq window-divider-default-right-width 1 window-divider-default-places 'right-only)
(window-divider-mode)


(setq custom-file null-device) ;; Set custom-file to null device

(defun disable-bold-and-italic-fonts ()
  "Disable bold and italic fonts globally in all buffers."
  (mapc (lambda (face)
          (set-face-attribute face nil :weight 'normal :slant 'normal))
        (face-list)))

(add-hook 'after-init-hook 'disable-bold-and-italic-fonts)
