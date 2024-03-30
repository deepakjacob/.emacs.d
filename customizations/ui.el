(use-package evil
  :ensure t
  :config
  (evil-mode 1))

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

(use-package mini-frame
  :ensure t
  :config
  (mini-frame-mode 1)
  (setq mini-frame-show-parameters
        '((top . 0)
          (width . 0.7)
          (left . 0.5))))

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
