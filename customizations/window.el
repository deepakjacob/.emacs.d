;;; window.el --- window management configuration
;;; Commentary:

;;; Code:
(winum-mode)

;;; provide help on shortcuts
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x 5"
                                     "C-c ;"
                                     "C-c C-a"
                                     "C-c a"
                                     "C-x n"
                                     "C-x C-r"
                                     "C-x r"
                                     "M-s"
                                     "C-h"
                                     "C-x"
                                     "C-c"
                                     "C-x 4"
                                     "C-c ; f"
                                     "C-c ' f"
                                     ))

(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)
(add-hook 'after-init-hook 'guide-key-mode)

;;; arrange two windows side-by-side or up/down fashion
;;; initially both will be 50/50 size
;;; use shortcut C-x-4-t
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
            (next-win-buffer (window-buffer (next-window)))
            (this-win-edges (window-edges (selected-window)))
            (next-win-edges (window-edges (next-window)))
            (this-win-2nd (not (and (<= (car this-win-edges)
                                        (car next-win-edges))
                                    (<= (cadr this-win-edges)
                                        (cadr next-win-edges)))))
            (splitter
             (if (= (car this-win-edges)
                    (car (window-edges (next-window))))
                 'split-window-horizontally
               'split-window-vertically)))
       (delete-other-windows)
       (let ((first-win (selected-window)))
         (funcall splitter)
         (if this-win-2nd (other-window 1))
         (set-window-buffer (selected-window) this-win-buffer)
         (set-window-buffer (next-window) next-win-buffer)
         (select-window first-win)
         (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

;;; window.el ends here
