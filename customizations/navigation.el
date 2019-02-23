;;; navigation.el --- customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

;;; Commentary:

;;; Code:

;;; "When several buffers visit identically-named files,
;;; Emacs must give the buffers distinct names. The usual method
;;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;;; of the buffer names (all but one of them).
;;; The forward naming method includes part of the file's directory
;;; name at the beginning of the buffer name
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html


;;; Enhances M-x to allow easier execution of commands. Provides
;;; a filterable list of possible commands in the minibuffer
;;; http://www.emacswiki.org/emacs/Smex

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
(add-to-list 'projectile-globally-ignored-directories "functions/node_modules")
(add-to-list 'projectile-globally-ignored-directories "src/mocks/node_modules")
(add-to-list 'projectile-globally-ignored-directories "build/*")
(add-to-list 'projectile-globally-ignored-directories "public/static/*")

;;; navigation.el ends here
