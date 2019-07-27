(deftheme JD_Night
  "Created 2019-05-16.")

(custom-theme-set-faces
 'JD_Night
 '(default ((t (:family "Roboto Mono for Powerline" :foundry "nil" :width normal :height 130 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "#c5c8c6" :background "#1f1f1f" :stipple nil :inherit nil))))
 '(cursor ((t (:background "#41728e"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "#41728e"))))
 '(highlight ((t (:background "#040404" :foreground "#0d0d0d"))))
 '(region ((t (:background "#333537"))))
 '(shadow ((t (:foreground "#5c5e5e"))))
 '(secondary-selection ((t (:background "#5a5b5a"))))
 '(trailing-whitespace ((t (:background "#cc6666"))))
 '(font-lock-builtin-face ((t (:foreground "#81a2be"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#5a5b5a"))))
 '(font-lock-constant-face ((t (:foreground "#de935f"))))
 '(font-lock-doc-face ((t (:foreground "#717171" :inherit (font-lock-comment-face)))))
 '(font-lock-function-name-face ((t (:foreground "#81a2be"))))
 '(font-lock-keyword-face ((t (:foreground "#b294bb"))))
 '(font-lock-negation-char-face ((t (:foreground "#c5c8c6" :inherit (bold)))))
 '(font-lock-preprocessor-face ((t (:foreground "#c5c8c6" :inherit (bold)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#c5c8c6" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#c5c8c6" :inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#b5bd68"))))
 '(font-lock-type-face ((t (:foreground "#f0c674"))))
 '(font-lock-variable-name-face ((t (:foreground "#cc6666"))))
 '(font-lock-warning-face ((t (:inherit (warning)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:weight bold :underline (:color foreground-color :style line) :foreground "#41728e"))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:foreground "#3f4040" :inherit (default)))))
 '(header-line ((t (:inherit (mode-line)))))
 '(tooltip ((t (:foreground "#c5c8c6" :background "#292b2b"))))
 '(mode-line ((t (:box nil :foreground "#ffffff" :background "#0f1011"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:foreground "#41728e"))))
 '(mode-line-highlight ((t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box nil :foreground "#5a5b5a" :background "#1d1f21"))))
 '(isearch ((t (:weight bold :inherit (lazy-highlight)))))
 '(isearch-fail ((t (:weight bold :foreground "#0d0d0d" :background "#cc6666"))))
 '(lazy-highlight ((t (:weight bold :foreground "#ffffff" :background "#41728e"))))
 '(match ((t (:weight bold :foreground "#b5bd68" :background "#0d0d0d"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'JD_Night)
