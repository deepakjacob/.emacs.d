
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))
