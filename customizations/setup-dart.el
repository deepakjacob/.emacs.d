;;; Dart specific config

(add-to-list 'load-path "~/.emacs.d/lisp/dart-mode/")
(add-to-list 'load-path "~/.emacs.d/lisp/dash/")
(add-to-list 'load-path "~/.emacs.d/lisp/s/")
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
(autoload 'dart-mode "dart-mode")

(setq dart-sdk-path "/Users/jacobdeepak/Development/flutter/bin/cache/dart-sdk/")
(setq dart-enable-analysis-server t)
(setq dart-format-on-save t)
