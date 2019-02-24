;;; enhancements.el --- miscellaneous configs
;;; Commentary:

;;; Code:
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path - specfified by FPATH.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath)) backupFilePath))

;;; enhancements.el ends here
