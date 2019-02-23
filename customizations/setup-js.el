;;; setup-js.el --- editing goodies for node and JS
;;; Commentary:

;;; Code:
(use-package json-mode
  :init
  (setq-default json-reformat:indent-width 2)
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . json-mode))
  :ensure t)

(use-package company :ensure t)

(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode)
                             (company-mode)))
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  :ensure t)

(use-package xref-js2
  :config
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :ensure t)

(use-package rjsx-mode
  :config
  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset 2)
  (setq-default js2-highlight-level 2)
  (setq-default js2-strict-missing-semi-warning nil)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  :ensure t)

(use-package js2-refactor
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  :ensure t
  )


;; (eval-after-load 'js-mode
	;; '(add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;; (eval-after-load 'js2-mode
  ;; '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;;; setup-js.el ends here
