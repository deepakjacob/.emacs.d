(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

(define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)
;; '(lsp-enable-snippet nil)
;; '(lsp-ui-doc-delay 1)
;; '(lsp-ui-doc-max-height 8)
;; '(lsp-ui-sideline-delay 2)
;; '(lsp-ui-sideline-show-code-actions nil)
;; '(lsp-ui-sideline-show-hover nil)



;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))
