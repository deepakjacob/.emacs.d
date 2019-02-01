;; javascript / html
;;better javascript + jsx
(use-package rjsx-mode
  :defer
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; tab width for js2-mode
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default js2-highlight-level 2)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default json-reformat:indent-width 2)
;; refactoring support
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; for files 'like' json
(use-package json-mode
  :defer
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.ejs\\'" . json-mode))


;;(require 'lsp-mode)
;;(add-hook 'rjsx-mode-hook #'lsp)

;;(require 'lsp-ui)
;;(add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;(add-hook 'rjsx-mode-hook 'flycheck-mode)

;;(require 'company-lsp)
;;(push 'company-lsp company-backends)

;;(setq lsp-print-io t)


(require 'company)
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

(require 'xref-js2)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


(eval-after-load 'js-mode
	   '(add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

(eval-after-load 'js2-mode
	   '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))
