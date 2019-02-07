;; Pencil (https://github.com/f1u77y/emacs-pencil-theme)

;;; pencil-theme.el

;;; Code:

(deftheme pencil)

(defgroup pencil nil
  "Pencil color theme group")

(defcustom pencil/higher-contrast-ui nil
  "Higher contrast of various background colors"
  :type 'boolean
  :group 'pencil)

(defcustom pencil/italics t
  "Enables italics"
  :type 'boolean
  :group 'pencil)

(defcustom pencil/dark nil
  "Use dark variant"
  :type 'boolean
  :group 'pencil)

(let ((black           "#212121")
      (medium-gray     "#767676")
      (white           "#f1f1f1")
      (actual-white    "#ffffff")
      (light-black     "#424242")
      (lighter-black   "#545454")

      (subtle-black    (if pencil/higher-contrast-ui "#262626" "#303030"))
      (light-gray      (if pencil/higher-contrast-ui "#d9d9d9" "#b2b2b2"))
      (lighter-gray    (if pencil/higher-contrast-ui "#e5e6e6" "#c6c6c6"))

      (pink            "#fb007a")
      (dark-red        "#c30071")
      (light-red       "#e32791")
      (orange          "#d75f5f")

      (darker-blue     "#005f87")
      (dark-blue       "#008ec4")
      (blue            "#20bbfc")
      (light-blue      "#b6d6fd")
      (dark-cyan       "#20a5ba")
      (light-cyan      "#4fb8cc")

      (dark-green      "#10a778")
      (light-green     "#5fd7a7")

      (dark-purple     "#523c79")
      (light-purple    "#6855de")

      (yellow          "#f3e430")
      (dark-yellow     "#a89c14"))

  (let
      ((bg              (if pencil/dark black         white        ))
       (bg-subtle       (if pencil/dark light-black   light-gray   ))
       (bg-very-subtle  (if pencil/dark subtle-black  lighter-gray ))
       (norm            (if pencil/dark lighter-gray  light-black  ))
       (norm-subtle     (if pencil/dark light-gray    lighter-black))
       (purple          (if pencil/dark light-purple  dark-purple  ))
       (cyan            (if pencil/dark light-cyan    dark-cyan    ))
       (green           (if pencil/dark light-green   dark-green   ))
       (red             (if pencil/dark light-red     dark-red     ))
       (bg-region       (if pencil/dark lighter-black light-blue   ))
       (bg-highlight    (if pencil/dark light-black   light-cyan   )))
    (let
	((comment-face    `((t (:foreground ,medium-gray :slant italic))))
	 (constant-face   `((t (:foreground ,cyan  ))))
	 (identifier-face `((t (:foreground ,dark-blue))))
	 (statement-face  `((t (:foreground ,green))))
	 (preproc-face    `((t (:foreground ,red))))
	 (type-face       `((t (:foreground ,purple))))
	 (special-face    `((t (:foreground ,pink))))
	 (error-face      `((t (:foreground ,red :weight bold)))))

      (custom-theme-set-faces
       'pencil


       ;; Built-in stuff (Emacs 23)
       `(border ((t (:background ,bg-very-subtle))))
       `(cursor ((t (:background ,blue :foreground ,norm))))
       `(default ((t (:background ,bg :foreground ,norm))))
       `(fringe ((t (:background ,bg))))
       `(highlight ((t (:background ,bg-highlight))))
       `(link ((t (:foreground ,cyan))))
       `(link-visited ((t (:foreground ,purple))))
       `(minibuffer-prompt ((t (:foreground ,green :weight normal))))

       ;; linum-mode
       `(linum ((t (:foreground ,bg-subtle :background ,bg))))

       ;; Mode line
       `(mode-line ((t (:background ,bg-subtle :box nil))))
       `(mode-line-buffer-id ((t (:foreground ,norm))))
       `(mode-line-emphasis ((t (:foreground ,norm :slant italic))))
       `(mode-line-highlight ((t (:foreground ,norm :background ,green :box nil))))
       `(mode-line-inactive ((t (:foreground ,bg-very-subtle :box nil))))


       `(region ((t (:background ,bg-region))))
       `(secondary-selection ((t (:background ,bg-region))))
       `(error ((t (:foreground ,red :weight bold))))
       `(warning ((t (:foreground ,light-red :weight bold))))
       `(success ((t (:foreground ,green :weight bold))))

       `(header-line ((t (:inherit mode-line :background nil))))

       ;; Font-lock stuff
       `(font-lock-builtin-face              ,constant-face)
       `(font-lock-comment-delimiter-face    ,comment-face)
       `(font-lock-comment-face              ,comment-face)
       `(font-lock-constant-face             ,constant-face)
       `(font-lock-doc-face                  ,constant-face)
       `(font-lock-doc-string-face           ,constant-face)
       `(font-lock-function-name-face        ,identifier-face)
       `(font-lock-keyword-face              ,statement-face)
       `(font-lock-negation-char-face        ,special-face)
       `(font-lock-preprocessor-face         ,preproc-face)
       `(font-lock-regexp-grouping-backslash ,constant-face)
       `(font-lock-regexp-grouping-construct ,constant-face)
       `(font-lock-string-face               ,constant-face)
       `(font-lock-type-face                 ,type-face)
       `(font-lock-variable-name-face        ,identifier-face)
       `(font-lock-warning-face              ,error-face)


       ;; Search
       `(match ((t (:background ,bg-very-subtle : :inverse-video t))))
       `(isearch ((t (:foreground ,yellow :background ,light-black :inverse-video t))))
       `(isearch-lazy-highlight-face ((t (:background ,bg-subtle :inverse-video t))))
       `(isearch-fail ((t (:color ,light-red :inverse-video t))))

       ;; Popups
       `(popup-face ((t (:foreground ,norm-subtle :background ,bg-very-subtle))))
       `(popup-isearch-match ((t (:foreground ,norm-subtle :background ,yellow))))
       `(popup-scroll-bar-background-face ((t (:background ,norm-subtle))))
       `(popup-scroll-bar-foreground-face ((t (:background ,bg-subtle))))
;       `(popup-summary-face ((t (:foreground ,base03))))
       `(popup-tip-face ((t (:foreground ,norm-subtle :background ,bg-very-subtle))))
       `(popup-menu-mouse-face ((t (:foreground ,norm-subtle :background ,bg-region))))
       `(popup-menu-selection-face ((t (:foreground ,norm-subtle :background ,bg-region))))

       ))))
(provide-theme 'pencil)

;;; pencil-theme.el ends here
