;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)
;;tab width to 2
(setq-default tab-width 2)

;;rainbow parens!
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

;;multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-." . mc/mark-next-like-this))
)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(add-hook 'after-init-hook 'global-company-mode)

;; highlight-sysmbol configurations
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
(global-set-key [(control shift f3)] 'unhighlight-regexp)
(global-set-key [(control shift mouse-1)]
                (lambda (event)
                  (interactive "e")
                  (save-excursion
                    (goto-char (posn-point (event-start event)))
                    (highlight-symbol-at-point))))

(electric-indent-mode)
(electric-pair-mode)

(require 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

;; (global-set-key [remap mark-word] 'mark-whole-word)

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key [remap mark-word] 'easy-mark)

(global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))

(setq ns-command-modifier 'meta)
(setq mac-option-modifier 'meta)


(define-globalized-minor-mode
    global-text-scale-mode
    text-scale-mode
    (lambda () (text-scale-mode 1)))

  (defun global-text-scale-adjust (inc) (interactive)
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(require 'goto-chg)
