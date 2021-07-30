
(setq ns-command-modifier 'meta)
(setq mac-option-modifier 'meta)


(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

;; Use swiper and ivy mode for completion
;; Use swiper for navigation
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; projectile everywhere!
(projectile-mode)
;; projectile defect fix
(setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))))
;; use ivy as the completion backend for projectile
(setq projectile-completion-system 'ivy)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; ignore node_modules folder
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(add-to-list 'projectile-globally-ignored-directories "client/x-app/node_modules")
(add-to-list 'projectile-globally-ignored-directories "**/*/node_modules")
(add-to-list 'projectile-globally-ignored-directories "functions/node_modules")
(add-to-list 'projectile-globally-ignored-directories "src/mocks/node_modules")
(add-to-list 'projectile-globally-ignored-directories "build/*")
(add-to-list 'projectile-globally-ignored-directories "public/static/*")


;; kill current buffer
(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-x") 'execute-extended-command)

(global-set-key (kbd "C-c s") 'surround-with)


(global-set-key (kbd "M-Y") 'browse-kill-ring)
;; TODO find out why this is not working
;; (push 'browse-kill-ring-mode page-break-lines-modes)
;; (turn-on-page-break-lines-mode)

(require 'move-dup)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c u") 'md/duplicate-up)

(global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))

(use-package neotree
  :defer t
  :bind
  (("C-c n" . neotree-toggle)
  ("C-c i" . neotree-dir)
  ("C-c f" . neotree-find))
  :config
  (setq-default neo-theme (quote ascii))
  (setq-default neo-window-fixed-size nil)
  (setq-default neo-window-position 'right)
  (setq-default neo-window-width 45)
  (setq-default doom-neotree-folder-size 1.0)
  )

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
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package easy-kill

  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark)
  (global-set-key [remap mark-word] #'easy-mark))

(use-package expand-region
  :bind (("C-=" . 'er/expand-region)))


(use-package avy
  :ensure t
  :bind (("C-." . avy-goto-word-or-subword-1)
         ("C-," . avy-goto-char)
         ("C-<" . avy-goto-line)))


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

(winum-mode)
;;; arrange two windows side-by-side or up/down fashion
;;; initially both will be 50/50 size
;;; use shortcut C-x-4-t
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
            (next-win-buffer (window-buffer (next-window)))
            (this-win-edges (window-edges (selected-window)))
            (next-win-edges (window-edges (next-window)))
            (this-win-2nd (not (and (<= (car this-win-edges)
                                        (car next-win-edges))
                                    (<= (cadr this-win-edges)
                                        (cadr next-win-edges)))))
            (splitter
             (if (= (car this-win-edges)
                    (car (window-edges (next-window))))
                 'split-window-horizontally
               'split-window-vertically)))
       (delete-other-windows)
       (let ((first-win (selected-window)))
         (funcall splitter)
         (if this-win-2nd (other-window 1))
         (set-window-buffer (selected-window) this-win-buffer)
         (set-window-buffer (next-window) next-win-buffer)
         (select-window first-win)
         (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)
