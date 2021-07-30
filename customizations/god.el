(use-package god-mode
  :ensure t
  :config
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  (add-hook 'post-command-hook 'my-god-mode-update-mode-line)
  (which-key-enable-god-mode-support))

;; You can change the cursor style to visually indicate
;; whether God mode is active as follows:
(defun my-god-mode-update-cursor-type () (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;; mode-line highlight
(defun my-god-mode-update-mode-line ()
  (cond
   (god-local-mode
    (set-face-attribute 'mode-line nil :foreground "#604000" :background "#fff29a")
    (set-face-attribute 'mode-line-inactive nil :foreground "#3f3000" :background "#fff3da"))
   (t
    (set-face-attribute 'mode-line nil :foreground "#0a0a0a" :background "#d7d7d7")
    (set-face-attribute 'mode-line-inactive nil	:foreground "#404148" :background "#efefef"))))
