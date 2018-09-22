(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c"
"C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r"
"M-s" "C-h" "C-c C-a" "C-c a"))

(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)
(add-hook 'after-init-hook 'guide-key-mode)


(use-package browse-kill-ring
  :ensure t
  :config (setq browse-kill-ring-separator "\f")

  :bind (
    ("M-n" . 'browse-kill-ring-forward)
    ("M-p" . 'browse-kill-ring-previous))
)

(global-set-key (kbd "M-Y") 'browse-kill-ring)
(push 'browse-kill-ring-mode page-break-lines-modes)

(require 'move-dup)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c u") 'md/duplicate-up)


(defun surround-with (text-begin text-end)
  "Surround current word or region with given text."
  (interactive "sStart : \nsEnd : ")
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (progn
          (goto-char (region-end))
          (insert text-end)
          (goto-char (region-beginning))
          (insert text-begin))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (goto-char (cdr bds))
        (insert text-end)
        (goto-char (car bds))
        (insert text-begin)))))

(global-set-key (kbd "C-c s") 'surround-with)

(require 'sgml-mode)

(defun jd/reformat-xml ()
  (interactive)
  (sgml-mode)
  (save-excursion
   (sgml-pretty-print (point-min) (point-max))
   (indent-region (point-min) (point-max))))

(global-set-key (kbd "C-c x") 'jd/reformat-xml)

(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "/Users/jacobdeepak/Development/rust/rust/src") ;; Rust source code PATH

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

