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

;; (dolist (x '(1 2 3)) ; test key
;;   (format "%s" (+ 100 x)))


;; Emacs lisp key enhancement
(let ((kms (list "TAB"		'completion-at-point
		 "C-c 1"	'macroexpand-list
		 "C-c C-1"	'macroexpand-list-inplace
		 "C-c 2" 	'macroexpand-block
		 "C-c C-2" 	'macroexpand-block-inplace ;For elisp: inplace more agile
		 "C-c C-3" 	'macroexpand-all)))
  (while kms
    (let ((k (pop kms))
	  (f (pop kms)))
      (define-key emacs-lisp-mode-map (kbd k) f)
      (define-key lisp-interaction-mode-map (kbd k) f)))) 
