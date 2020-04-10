(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))


;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (typescript-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))


(defun sort-js-imports ()
    (interactive)
    (sort-js-imports-region nil (point-min) (get-end-of-js-import))
    )

  (defun get-end-of-js-import ()
    (save-excursion
      (goto-char (point-max))
      (search-backward-regexp "import")
      (word-search-forward "from" nil t)
      (line-end-position)
      )
    )

  (defun sort-js-imports-region (reverse beg end)
    (interactive "P\nr")
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (let ;; To make `end-of-line' and etc. to ignore fields.
            ((inhibit-field-text-motion t))
          (sort-subr reverse
                     'forward-line
                     (lambda () (search-forward "from" nil t) (end-of-line))
                     (lambda ()
                       (let (
                             (skip-amount (save-excursion (search-forward "from" nil t)))
                             (skip-amount-end (line-end-position))
                             )
                         (cons skip-amount skip-amount-end)
                         )
                       )
                     nil
                     (lambda (a b)
                       (let ((s1 (buffer-substring (car a) (cdr a)))
                             (s2 (buffer-substring (car b) (cdr b))))
                         (message (format "[%s] [%s] [%s] [%s] [%s]\n" s1 s2 a b (calculate-string-priority s1 s2)))
                         (calculate-string-priority s1 s2)
                       )
                     ))))))

  (defun calculate-string-priority (s1 s2)
    (< (+ (calculate-js-import-priority s1) (if (string< s1 s2) -1 1)) (calculate-js-import-priority s2)))

  (defun calculate-js-import-priority (import)
    (catch 'val
      (cond
       ((> (or (string-match "react" import) -2) -1) (throw 'val 0))
       ((> (or (string-match "redux" import) -2) -1) (throw 'val 20))
       ((> (or (string-match "redux-promise-middleware" import) -2) -1) (throw 'val 30))
       ((> (or (string-match "@material-ui" import) -2) -1) (throw 'val 40))
       ((> (or (string-match "/containers/" import) -2) -1) (throw 'val 50))
       ((> (or (string-match "/components/pages/" import) -2) -1) (throw 'val 60))
       ((> (or (string-match "/components/" import) -2) -1) (throw 'val 70))
       ((> (or (string-match "/components/store/actions/" import) -2) -1) (throw 'val 80))
       ((> (or (string-match "/components/store/reducers/" import) -2) -1) (throw 'val 90))
       ((> (or (string-match "/services/" import) -2) -1) (throw 'val 100))
       ((> (or (string-match "/selectors/" import) -2) -1) (throw 'val 110))
       ((> (or (string-match "/components/hocs/" import) -2) -1) (throw 'val 120))
       ((> (or (string-match "scss" import) -2) -1) (throw 'val 130))
       ((> (or (string-match "./" import) -2) -1) (throw 'val 140))
       ((> (or (string-match "../" import) -2) -1) (throw 'val 150))
       ((> (or (string-match "../../../" import) -2) -1) (throw 'val 160))
       ((> (or (string-match "../store/" import) -2) -1) (throw 'val 170))
       ((> (or (string-match "'../types'" import) -2) -1) (throw 'val 180))
       (t (throw 'val 200)))
      )
    )

  ;; (add-hook 'typescript-mode-hook
  ;;          (lambda () (add-hook 'before-save-hook #'sort-js-imports nil 'local)))

;;  (add-hook 'rjsx-mode-hook
;;            (lambda () (add-hook 'before-save-hook #'sort-js-imports nil 'local)))
