(use-package diminish :ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package neotree
  :defer t
  :bind
  (("C-c n" . neotree-toggle)
  ("C-c i" . neotree-dir)
  ("C-c f" . neotree-find))
  :config
  (setq-default neo-theme (quote ascii))
  (setq-default neo-window-fixed-size nil)
  (setq-default neo-window-position 'right)
  (setq-default neo-window-width 45)
  (setq-default doom-neotree-folder-size 1.0)
  )

(use-package magit
  :ensure t
  :bind
  (("C-c m" . 'magit-status)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)) ;; default 0.5
