;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ;; ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    counsel

    ;; for general completion
    ivy

    ;; for navigtion
    swiper

    ;; for react
    rjsx-mode

    ;; for json
    json-mode

    ;; zerodark theme
    zerodark-theme

    ;; spacemacs
    spacemacs-theme

    ;; ace-window
    ;; use C-x-o to navigate windows within a frame
    windmove


    ;; expand-region
    expand-region

    ;;dump-jump
    dumb-jump

    ;;neotree
    neotree

    page-break-lines

    smooth-scrolling

    company

    multiple-cursors

    js2-refactor

    use-package

    doom-themes

    ;; highlight symbols
    highlight-symbol

    ;; easy loading of packages
    use-package

    ;; display keyboard shortcuts
    guide-key

    ;; see kill ring contents
    browse-kill-ring

    ;; duplicate region above / below
    move-dup

    ;; remove clutter while displaying minor modes
    diminish

    ;; window switching
    winum

    jbeans-theme

    ;; for elixir / phoenix / mix
    alchemist

    ;; vim style tet deletion within '',"", ``, [], {}, () etc
    change-inner

    ;; markdown mode
    markdown-mode

    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
;;(load "setup-clojure.el")

(load "setup-js.el")

;; code formatting
(load "prettier-js.el")

(load "enhancements.el")

(global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))


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

(require 'powerline)
(powerline-default-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "DarkOliveGreen3" "#e7c547" "DeepSkyBlue1" "#c397d8" "#70c0b1" "#181a26"))
 '(custom-safe-themes
   (quote
    ("329bf20724e08693166b30de36e28e02ec181c793fbaea4d21d69dac96284e0b" "b52fd32d4e68aaf71279d7a79b11000187a6005a056c5899c4f1dfab71adebe0" "8ec049558b2f9375b0d7cfd0121d970357643662168a3dfbedfe2edabc55652c" "3021d4961b447f532aff13d6a1b0ffb3f75d6dbd0901b9836582f5cabd0e835d" "c5ebd43e0b169feb0fbfe6061f80986d2197f639faf34f6988e0c941f6a68246" "81ddf7e3643fff520ae55dd29b0119da209974775e3f016beb44673e04125903" "8c2256868e846a246e0d710aa872d6ac3167bfdbd13f5412ecf6cd3705619e4a" "bc14ed09a3cd8b88c5809678f78cd880c94a3d0631e75a99c505f45d0ffd5704" "216eaba8e37f1ffa6851f79b9b156f2eb02700b4bcc97b983db41ff941cfbe01" "220b3346bf2526f458010b19f2272887fdadfc81e61d0fe69a30aeddca8fc2a4" "58f14aa50eaf3573e85ef711dca330e464c2bfa1d052ec394ca584ec5b0e5871" "a1d0757eedd6704fa6c08a0c3304a79d01dd34667605c0ce34dc6b0567af8300" "c67839db3df89e732497d0d43d032da87ea2d1cef12253fa98f65e86a9d88535" "070136058961651892e48e9dfa018cdbe442d58c019304c1f54f5fe636df513a" "f65890c102079a475fd5f8cab0c1986bc024f54488605e4bfa0d5cc8de5ec5f1" "63fc11a233af3968f90969d684f3412685216a8a74760aeaac393ec6e06b877b" "181abc89b4bb03d8e38955b8482d2d4ba01d51d0f8f7c3b099a2f147281d0a2f" "e01fee5419a7757923b1fb500b4ecb824ee7539421e1e6afb588e8a33f348e02" "6284902410d8b2a39b8c1918375794c1caa49a829b51f37acf9f6efcf14216cc" "016c01256c26cc6f69293264610cc2e3f4c20b2c9a88dc2edf33731ba1e6e11c" "121e867e62bd559beb5a454f9b635ec735369cd7090d884c636804635e5b00e9" "ee6f6e5fd8abb50b3930f85532e045ef495973ec485c52be3d7ae484236fb54a" "549e54adea89bb8ff4ce55e6298fa2eaba173e23a84344d404e30d2cd8a356c8" "0ba40a573e37733a615d987c2f0e9d95bf00b86202e5edecccaae1927632b32a" "2e942c3548de9c2e80d09907cc6b960958e40886873d3247b81a9bca3be06bba" "b60a9ecc3377e8a856de0e148f353e8c1c6885b0db5e8181314cbcc9aec8858d" "cccfb362216c2e224e77a74974380770402121b16d6ce11727cf930087e572ce" "ecaf5a0d0d45d73d9c22db5dff39f3c9d0117d8fbb4b64a351cca540f1db533e" "6d8049e67b79907fba461659aad9a7c2e30e9d3c7e77954987f18d22385d9212" "79cc7f3f3ebca3195e075c878b0aeb2c9aa60ee6c3e8b5c43884b7179e4f795d" "624c1f6316035159aa784904572a2d31b3a6e0395584f0f67933965523bbcc60" "78ac8af89a2551204f1284bf2102d1fc4188f38d88e0f4f60b1d7b62c81a9365" "ecb222cc4f241f22ba1df3e5b773bee211875c9f95d880ba4434ae1070320257" "3b5ce826b9c9f455b7c4c8bff22c020779383a12f2f57bf2eb25139244bb7290" "4f2ede02b3324c2f788f4e0bad77f7ebc1874eff7971d2a2c9b9724a50fb3f65" "c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" "8cb48d15dac4ac08bc97cff5a84554833a6140c8907f50d1e5de755f3b324b6d" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "2e1d19424153d41462ad31144549efa41f55dacda9b76571f73904612b15fd0a" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "3481e594ae6866d72c40ad77d86a1ffa338d01daa9eb0977e324f365cef4f47c" "d0404bd38534a00ee72a4f887a987d6bff87f4cf8d8f85149e32849b262465a5" "73e35ffa5ca98b57a9923954f296c3854ce6d8736b31fdbdda3d27502d4b4d69" "a7e7804313dbf827a441c86a8109ef5b64b03011383322cbdbf646eb02692f76" "0a3a41085c19d8121ed0ad3eb658a475ccb948a70a83604641ee7d4c3575a4d5" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "42b8102c1234a9f680722953161c1127cc59ec68ad8d5c710af60d68c3b6e6ef" default)))
 '(doom-neotree-folder-size 1.0)
 '(fci-rule-color "#14151E")
 '(fringe-mode 6 nil (fringe))
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(linum-format " %3i ")
 '(neo-theme (quote ascii))
 '(neo-window-fixed-size nil)
 '(neo-window-position (quote right))
 '(neo-window-width 45)
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (2048-game github-theme heroku-theme color-theme-heroku evil-search-highlight-persist hydandata-light-theme dakrone-light-theme autumn-light-theme cherry-blossom-theme afternoon-theme markdown-mode change-inner alchemist jbeans-theme winum diminish move-dup browse-kill-ring guide-key highlight-symbol doom-themes zerodark-theme use-package tagedit spacemacs-theme smooth-scrolling smex rjsx-mode rainbow-delimiters projectile paredit page-break-lines neotree json-mode js2-refactor expand-region exec-path-from-shell dumb-jump counsel company clojure-mode-extra-font-locking cider ace-window)))
 '(powerline-default-separator (quote slant))
 '(powerline-gui-use-vcs-glyph t)
 '(powerline-image-apple-rgb t)
 '(tooltip-recent-seconds 1)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active0 ((t (:inherit mode-line :background "gray0" :foreground "#9CDCFE"))))
 '(powerline-active1 ((t (:inherit mode-line :background "gray0" :foreground "#ce9178"))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray0"))))
 '(powerline-inactive0 ((t (:inherit mode-line-inactive :background "gray0"))))
 '(powerline-inactive1 ((t (:inherit mode-line :background "gray0" :foreground "gray100")))))
