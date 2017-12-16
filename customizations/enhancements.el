(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c"
"C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r"
"M-s" "C-h" "C-c C-a"))

 (add-hook 'after-init-hook 'guide-key-mode)


(use-package browse-kill-ring
  :ensure t
  :config (setq browse-kill-ring-separator "\f")

  :bind (
    ("C-g" . 'browse-kill-ring-quit)
    ("M-n" . 'browse-kill-ring-forward)
    ("M-p" . 'browse-kill-ring-previous))
)

(global-set-key (kbd "M-Y") 'browse-kill-ring)
(push 'browse-kill-ring-mode page-break-lines-modes)





(require 'move-dup)
;;(global-set-key [M-up] 'md/move-lines-up)
;;(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c u") 'md/duplicate-up)
