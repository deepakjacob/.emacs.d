
(define-key mode-line-major-mode-keymap [header-line]
    (lookup-key mode-line-major-mode-keymap [mode-line])
)

(defun mode-line-render (left right)
    (let* ((available-width (- (window-width) (length left) )))
        (format (format "%%s %%%ds" available-width) left right)))
(setq-default mode-line-format
    '((:eval (mode-line-render
        (format-mode-line (list
            (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                'help-echo "Mode(s) menu"
                'mouse-face 'mode-line-highlight
                'local-map   mode-line-major-mode-keymap)
            " %b "
            (if (and buffer-file-name (buffer-modified-p))
                (propertize "(modified)" 'face `(:inherit face-faded)))))
       (format-mode-line (propertize "%4l:%2c  " 'face `(:inherit face-faded)))))))


;; Comment if you want to keep the modeline at the bottom
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)