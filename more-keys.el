;; Global helper functions
(defun kill-other-buffer ()
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window -1))

(defun recenter-top-bottom-other-window ()
  (interactive)
  (save-excursion
    (other-window 1)
    (recenter-top-bottom)
    (other-window -1)))

(defun kill-matching-buffers-without-ask (regexp &optional internal-too) 
  "Modified version of built-in function 'kill-matching-buffers"
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(defun kill-helper-buffers (&rest args)
  "Kill all temporary helper buffers."
  (interactive)
  (dolist (pat (list "[Mm]acroexp"
		     "[Hh]elp"
		     "[Cc]ompletion"
		     "[Dd]oc"
		     "[Dd]escript"
		     "[Ee]rror"
		     "[Ww]arn"
		     ))
    (kill-matching-buffers-without-ask
     (concat pat "\\S+\*"))))

(defun mark-till-here (ARG)
  (interactive "P")
  (let ((phere (point)))
    (goto-char 0)
    (set-mark-command ARG)
    (goto-char phere)))

(global-set-key (kbd "C-S-SPC")		'mark-till-here)

(global-set-key (kbd "C-x 4 k")		'kill-other-buffer)
(global-set-key (kbd "C-x p")		'(lambda ()
					   (interactive)
					   (other-window -1)))
(global-set-key (kbd "C-x \\")		'kill-helper-buffers)

;; Enhancement for parenthesis

(global-set-key (kbd "<apps> <apps>") 	nil)
(global-set-key (kbd "<apps> l") 	'toggle-truncate-lines)
(global-set-key (kbd "<apps> m") 	'toggle-menu-bar-mode-from-frame)

(global-set-key (kbd "C-<up>") '(lambda () (interactive)
				  (scroll-up 2)))
(global-set-key (kbd "C-<down>") '(lambda () (interactive)
				    (scroll-down 2)))


;;; Representative level controls.

(defun change-face-size (ARG)
  (interactive "nInput face height: ")
  (set-face-attribute 'default nil :height ARG)) 

(defun increase-face-size (x)
  (let ((h (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ h x))))

(defun set-transparency (x)
  (set-frame-parameter nil 'alpha (list x x)))

(defun change-transparency (ARG)
  (interactive "nInput alpha value: ")
  (set-transparency ARG))

(defun hs ()
  (interactive)
  (hs-minor-mode))

(global-set-key (kbd "<apps> t") 	'change-transparency)
(global-set-key (kbd "<apps> ,") 	'(lambda () (interactive)
					   (set-transparency 20)))
(global-set-key (kbd "<apps> .") 	'(lambda () (interactive)
					   (set-transparency 90)))
(global-set-key (kbd "<apps> f") 	'change-face-size)
(global-set-key (kbd "C-=") 		'(lambda () (interactive)
					   (increase-face-size 5)))
(global-set-key (kbd "C--")		'(lambda () (interactive)
					   (increase-face-size -5)))

(global-set-key (kbd "C-M-[") '(lambda (arg)
				 (interactive "P")
				 (insert-pair arg ?\[ ?\])))
(global-set-key (kbd "C-M-{") '(lambda (arg)
				 (interactive "P")
				 (insert-pair arg ?\{ ?\})))
(global-set-key (kbd "C-M-'") '(lambda (arg)
				 (interactive "P")
				 (insert-pair arg ?\' ?\')))
(global-set-key (kbd "C-M-\"") '(lambda (arg)
				 (interactive "P")
				 (insert-pair arg ?\" ?\")))

(global-set-key (kbd "C-S-l") 'recenter-top-bottom-other-window)
