;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
;;(setq shell-file-name "/bin/bash")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))
