;;; editing.el --- editing goodies
;;; Commentary:

;;; Code:
(setq ns-command-modifier 'meta)
(setq mac-option-modifier 'meta)

(show-paren-mode 1)
(global-hl-line-mode 1)

(electric-pair-mode)
(electric-indent-mode)

(global-set-key (kbd "C-c s") 'surround-with)

(setq-default tab-width 2)
(setq auto-save-default nil)
(setq-default indent-tabs-mode nil)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(global-set-key (kbd "M-Y") 'browse-kill-ring)
(push 'browse-kill-ring-mode page-break-lines-modes)

(require 'move-dup)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c u") 'md/duplicate-up)

(global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))


(use-package magit
  :ensure t
  :bind
  (("C-c m" . 'magit-status)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package browse-kill-ring
  :ensure t
  :config (setq browse-kill-ring-separator "\f")

  :bind (
         ("M-n" . 'browse-kill-ring-forward)
         ("M-p" . 'browse-kill-ring-previous)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-." . mc/mark-next-like-this)))

(use-package hippie-expand
  :init
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-lisp-symbol-partially
          try-complete-file-name
          try-expand-dabbrev
          try-complete-lisp-symbol
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill))
  :bind
  ("M-/" . hippie-expand))

(use-package change-inner
  :bind (("M-i"     . change-inner)
         ("M-o" . change-outer)))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark)
  (global-set-key [remap mark-word] #'easy-mark))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)

(use-package expand-region
  :bind (("C-=" . 'er/expand-region)))

 (use-package ace-jump-buffer
    :ensure t
    :bind
    ("C-," . ace-jump-buffer))
;;
;; ace jump mode major function
;;
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


(defun toggle-comment-on-line ()
  "Toggle comments on or off on the line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))


(defun global-text-scale-adjust (inc)
  "Increase or decrease font size.
INC the amount of increase or decrease"
  (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1))

(global-set-key (kbd "M-0")
                '(lambda () (interactive)
                   (global-text-scale-adjust (- text-scale-mode-amount))
                   (global-text-scale-mode -1)))
(global-set-key (kbd "M-+")
                '(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M-_")
                '(lambda () (interactive) (global-text-scale-adjust -1)))


(defun surround-with (text-begin text-end)
  "Surround current word or region with given text.
TEXT-BEGIN / TEXT-END are start tag and end tag respectively"
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

;;;

(defun jd/reformat-xml ()
  "Reformat XML."
  (interactive)
  (sgml-mode)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(global-set-key (kbd "C-c x") 'jd/reformat-xml)


;;; editing ends here
