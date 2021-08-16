(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp)
         (rjsx-mode .lsp)
         (typescript-mode .lsp)f
         ;;(go-mode . lsp-go-install-save-hooks)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)

         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :custom;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
          (lsp-ui-doc-show-with-cursor nil)
          (lsp-lens-enable nil)
          (lsp-headerline-breadcrumb-enable nil)
          (lsp-ui-doc-enable nil)
          (lsp-ui-sideline-enable nil)
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
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

;; for any resons if before-save doesn't work for other languages
;; disable before-save mode with in use-package hook and enable
;; following line and uncomment lsp-go-install-save-hook within use package

;;(defun lsp-go-install-save-hooks ()
;;  (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;  (add-hook 'before-save-hook #'lsp-organize-imports t t))
;;(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))
