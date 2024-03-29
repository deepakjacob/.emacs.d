(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)


(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 90))

(set-frame-font "Fira Code-11" nil t)
;;(set-frame-font "Comic Code Ligatures-12" nil t)
;; Remove the menu bar and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Projectile
(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(require 'projectile)

;;; Enable Projectile and configure it to remember recent projects
(projectile-mode +1)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-indexing-method 'alien)
(setq projectile-completion-system 'ivy)

;;; Create a function to display recent projects on startup
(defun display-projectile-list-on-startup ()
  (let ((buf (get-buffer-create "*Projectile Projects*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (project (projectile-relevant-known-projects))
        (insert project "\n")))
    (switch-to-buffer buf)))

	
;; Mini-frame
(unless (package-installed-p 'mini-frame)
  (package-install 'mini-frame))

(require 'mini-frame)
(mini-frame-mode 1)

(setq mini-frame-show-parameters
      '((top . 0)
        (width . 0.7)
        (left . 0.5)))
;; Ivy support
(unless (package-installed-p 'ivy)
  (package-install 'ivy))

(unless (package-installed-p 'ivy-posframe)
  (package-install 'ivy-posframe))


(require 'ivy)
(ivy-mode 1)

(require 'ivy-posframe)
(setq ivy-display-function #'ivy-posframe-display-at-frame-center)
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
(ivy-posframe-mode 1)

(setq scroll-conservatively 101)
;; Speed bar
(dolist (package '(which-key general))
  (unless (package-installed-p package)
    (package-install package)))

(require 'which-key)
(which-key-mode)

(require 'general)
(general-create-definer my-leader-def
  :states '(normal visual insert emacs)
  :prefix "S-SPC"
  :non-normal-prefix "S-SPC")

(my-leader-def
  "p"  '(:ignore t :which-key "projects")
  "pf" 'projectile-find-file)

(setq which-key-idle-delay 0.5) ;; default 0.5


;; Modus operandi
(load-theme 'modus-operandi t)

;; Buffer splitting
(defun split-right-and-list-project-files ()
  "Split the current window into two, left and right. The left one contains the current contents and the right one contains a projectile-list of all the files in the project."
  (interactive)
  (split-window-right)
  (other-window 1)
  (projectile-find-file))

(defun split-below-and-list-project-files ()
  "Split the current window into two, top and bottom. The top one contains the current contents and the bottom one contains a projectile-list of all the files in the project."
  (interactive)
  (split-window-below)
  (other-window 1)
  (projectile-find-file))

(my-leader-def
  "b"  '(:ignore t :which-key "Buffers")
  "bs" '(:ignore t :which-key "Split")
  "bsr" '(split-right-and-list-project-files :which-key "Right split")
  "bst" '(split-below-and-list-project-files :which-key "Top split"))

;; Rust
(unless (package-installed-p 'rustic)
  (package-install 'rustic))

(require 'rustic)

;; use rust-analyzer as the LSP server
(setq rustic-lsp-server 'rust-analyzer) 

;;; Company
(unless (package-installed-p 'company)
  (package-install 'company))

(require 'company)
;; Enable company mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)
;; Set company backends for lsp-mode
(push 'company-capf company-backends)

;;; lsp-mode
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))

(require 'lsp-mode)
;; Enable lsp-mode in rustic-mode
(add-hook 'rustic-mode-hook #'lsp)
;; Use company-capf as the completion provider
(add-hook 'lsp-mode-hook (lambda () (set (make-local-variable 'company-backends) '(company-capf))))

;; Enable rustfmt on save
(setq rustic-format-on-save t)

;; Enable inlay hints
(setq lsp-rust-analyzer-display-inlay-hints t)
(setq lsp-inlay-hints-mode t)

;;; Speedbar for Rust
(my-leader-def
  :states '(normal visual emacs)
  :keymaps 'override
  "l" '(:ignore t :which-key "Language")
  "r" '(:ignore t :which-key "Rust"))

(my-leader-def
  :states '(normal visual emacs)
  :keymaps 'override
  "l r b" '(rustic-cargo-build :which-key "Build")
  "l r r" '(rustic-cargo-run :which-key "Run")
  "l r t" '(rustic-cargo-test :which-key "Test")
  "l r f" '(rustic-cargo-fmt :which-key "Format")
  "l r c" '(rustic-cargo-check :which-key "Check")
  "l r p" '(rustic-cargo-clippy :which-key "Clippy"))

  ;;disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
