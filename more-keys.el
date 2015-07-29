;; Global helper functions
(defun kill-other-buffer ()
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window -1))

(defun move-forward-paren (&optional arg)
  "Move to the first halp-paren afterward, then jump over it."
  (interactive "P")
  (while (not (looking-at "[][\'\"\(\)\{\}]")) ; Matching brackets. [][...]
    (forward-char 1))
  (forward-char 1))

(defun move-backward-paren (&optional arg)
  (interactive "P")
  (backward-char 1)
  (while (not (looking-back "[]\'[\"\(\)\{\}]"))
    (backward-char 1)))

(defun find-long-words-backward (n)
  (let ((end (progn (skip-syntax-forward "w_")
		    (point))))
    (let ((beg (dotimes (i n (point)) 
		 (skip-syntax-backward "w_")
		 (when (< i (1- n))
		   (skip-synwax-backward " ")))))
      (values beg end))))

(defun select-long-word ()
  (interactive)
  (destructuring-bind (beg end) (find-long-words-backward 1)
    (let ((word (buffer-substring-no-properties beg end)))
      (set-mark beg)
      (forward-char (- end beg)))))

;; Extend emacs lisp utilities 
(defun eval-list ()
  (interactive)
  (while (not (looking-at "\(")) (backward-char 1))
  (let ((s (sexp-at-point)))
    (eval-expression s)))

(defun macroexpand-list ()
  (interactive)
  (while (not (looking-at "\(")) (backward-char 1))
  (let ((s (sexp-at-point)))
    (with-output-to-temp-buffer "*el-macroexpansion*" 
      ;; (add-hook 'temp-buffer-setup-hook 'emacs-lisp-mode)
      (add-hook 'temp-buffer-show-hook 'emacs-lisp-mode)
      (pp (macroexpand s)))))

(defun macroexpand-list-inplace ()
  "Expand elisp macro list inplace"
  (interactive)
  (while (not (looking-at "\(")) (backward-char 1))
  (let ((s (sexp-at-point)))
    (kill-sexp)
    (insert (pp-to-string (macroexpand s)))))

(defun macroexpand-block ()
  (interactive)
  (search-backward-regexp "^\(")
  (let ((s (sexp-at-point)))
    (with-output-to-temp-buffer "*el-macroexpansion*" 
      ;; (add-hook 'temp-buffer-setup-hook 'emacs-lisp-mode)
      (add-hook 'temp-buffer-show-hook 'lisp-interaction-mode);'emacs-lisp-mode)
      (pp (macroexpand s))) 
    ))

(defun macroexpand-block-inplace ()
  "Expand elisp macro block (rather than a single sexp at point) inplace."
  (interactive)
  (when (looking-at "(") (forward-char 1))
  (search-backward-regexp "^\(")
  (let ((s (sexp-at-point)))
    (kill-sexp)
    (insert (pp-to-string (macroexpand s)))
    (search-backward-regexp "^\(")))

(defun document-show-at-point (&rest args)
  (let ((s (sexp-at-point)))
    (documentation (format "'%s" s))))

;; (dolist (x '(1 2 3)) ; test key
;;   (format "%s" (+ 100 x)))

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
    (goto-line 0)
    (set-mark-command ARG)
    (goto-char phere)))

;; Emacs lisp key enhancement
(define-keys
  (list emacs-lisp-mode-map
	lisp-interaction-mode-map)
  (list "TAB"		'completion-at-point
	"C-c 1"		'macroexpand-list
	"C-c C-1"	'macroexpand-list-inplace
	"C-c 2" 	'macroexpand-block
	"C-c C-2" 	'macroexpand-block-inplace ;For elisp: inplace more agile
	"C-c C-3" 	'macroexpand-all
	"C-M-j"		'eval-print-last-sexp
	)) 

;; Further settings.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(global-set-key (kbd "C-S-SPC")		'mark-till-here)

(global-set-key (kbd "C-x 4 k")		'kill-other-buffer)
(global-set-key (kbd "C-x O")		'(lambda ()
					   (interactive)
					   (other-window -1)))
(global-set-key (kbd "C-x g")		'switch-to-repl)
(global-set-key (kbd "C-x w")		'kill-helper-buffers)

;; Enhancement for parenthesis
(global-set-key (kbd "C->") 'move-forward-paren)
(global-set-key (kbd "C-,") 'move-forward-paren)
(global-set-key (kbd "C-<") 'move-backward-paren)

(global-set-key (kbd "<apps> <apps>") 	nil)
(global-set-key (kbd "<apps> l") 	'toggle-truncate-lines)
(global-set-key (kbd "<apps> m") 	'toggle-menu-bar-mode-from-frame)



;;; Representative level controls.

(defun change-face-size (ARG)
  (interactive "nInput face height: ")
  (set-face-attribute 'default nil :height ARG)) 

(defun increase-face-size (x)
  (let ((h (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ h x))))

(defun change-transparency (ARG)
  (interactive "nInput alpha value: ")
  (let* ((p (frame-parameter nil 'alpha))
	 (a (car p)))
    (set-frame-parameter nil 'alpha
			 (list ARG ARG))))

(global-set-key (kbd "<apps> t") 	'change-transparency)
(global-set-key (kbd "<apps> f") 	'change-face-size)
(global-set-key (kbd "C-=") 		'(lambda () (interactive)
					   (increase-face-size 5)))
(global-set-key (kbd "C--")		'(lambda () (interactive)
					   (increase-face-size -5)))


(setq inhibit-startup-screen t)