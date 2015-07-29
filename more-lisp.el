(defvar *my-clisp-program* "C:/Tools/ccl/wx86cl.exe")

(defun slime-define-my-keys ()
  (define-keys
    (list lisp-mode-map)
    (list "RET" 	'newline-and-indent
	  "C-c i" 	'slime-inspect
	  "C-c d" 	'slime-eval-defun
	  "C-c e" 	'slime-eval-last-expression
	  "C-c f"	'slime-describe-function
	  "C-c z"	'slime-documentation
	  "C-c 1" 	'slime-macroexpand-1
	  "C-c C-1" 	'slime-macroexpand-1-inplace
	  "C-c C-2" 	'slime-macroexpand-again
	  "C-c C-3" 	'slime-macroexpand-all
	  "C-c q"	'(lambda ()		; Kill macroexpansion buffer
			   (interactive)
			   (kill-buffer (slime-buffer-name :macroexpansion))))))

(defun load-slime ()
  (interactive) 
  (require 'slime) 
  (load (expand-file-name
	 "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program
	*my-clisp-program*) 
  (add-hook 'slime
	    'slime-define-my-keys))

