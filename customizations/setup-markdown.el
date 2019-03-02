
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(custom-set-variables
 '(livedown-autostart nil) ; automatically open preview when opening markdown files
 '(livedown-open t)        ; automatically open the browser window
 '(livedown-port 1337)     ; port for livedown server
 '(livedown-browser nil))  ; browser to use

(require 'livedown)

(global-set-key (kbd "C-M-m") 'livedown-preview)
