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
;;tab width to 4
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
  ;; comment
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(add-hook 'after-init-hook 'global-company-mode)


;; highlight-sysmbol configurations
(global-set-key [(control f6)] 'highlight-symbol-at-point)
(global-set-key [f6] 'highlight-symbol-next)
(global-set-key [(shift f6)] 'highlight-symbol-prev)
(global-set-key [(meta f6)] 'highlight-symbol-query-replace)
(global-set-key [(control shift f6)] 'unhighlight-regexp)
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
