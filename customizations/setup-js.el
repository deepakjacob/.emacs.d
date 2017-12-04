;; javascript / html
;;better javascript + jsx
(use-package rjsx-mode
  :defer
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))

(setq js2-strict-missing-semi-warning nil)
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

;; for files 'like' json
(use-package json-mode
  :defer
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.ejs\\'" . json-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

(require 'company)
(require 'company-tern)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'rjsx-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
  (add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
)



