
(defun zen-mode-on ()
  ;; toogle zen mode on"
  (setq-default mode-line-format nil)
 (highlight-indent-guides-mode -1)
 (linum-mode -1)
 (fci-mode -1)
 (neotree-hide))

(defun zen-mode-off ()
  ;; toogle zen mode on"
  (setq-default mode-line-format t)
 (highlight-indent-guides-mode t)
 (linum-mode t)
 (fci-mode t)
 (neotree-hide))

(zen-mode-on)
