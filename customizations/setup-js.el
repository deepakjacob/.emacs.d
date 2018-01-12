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
