(use-package rustic
  :ensure t
   :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  :config
  (setq rustic-format-on-save t))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (rustic-mode . lsp)
  :config
  (setq lsp-rust-analyzer-display-inlay-hints t)
  (setq lsp-inlay-hints-mode t)
  ;; Additional LSP configurations
  ;; (Include other LSP settings from the original init.el here)
  )

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (push 'company-capf company-backends))

;; Add additional rust-specific keybindings and settings here 
