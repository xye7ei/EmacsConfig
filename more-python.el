(require 'python)

(defun python-open-shell-other-window ()
  (interactive)
  (switch-to-buffer-other-window "*Python*")
  (other-window -1))

(defun python-object-at-point ()
  (save-excursion
    (let ((rslt (buffer-substring-no-properties
		 (progn (while (looking-back "[\(\)]") (backward-char))
			(while (looking-back "[^ @\(\)]") (backward-char))
			(point))
		 (progn (skip-syntax-forward "w_.")
			(skip-syntax-backward ".")
			(point)))))
      rslt)))

(defun python-get-expression ()
  (save-excursion
    (progn
      (while (not (looking-back "[=\n#]")) (backward-char))
      (buffer-substring-no-properties
       (point)
       (line-end-position)))))

(defun stripped-line ()
  (save-excursion
    (buffer-substring-no-properties (progn
				      (beginning-of-line)
				      (while (looking-at "[\s#]") (forward-char))
				      (point))
				    (progn
				      (end-of-line)
				      (point)))))
(defun python-send-line ()
  (interactive)
  (save-excursion
    (python-shell-send-string (stripped-line))))

(defun python-send-expression ()
  (interactive)
  (let ((expr (python-get-expression)))
    (python-shell-send-string expr)))

(defun python-send-line-and-newline ()
  (interactive)
  (python-send-line)
  (newline))

(defun use-ipython ()
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""; "-i C:/Tools/Python33/Scripts/ipython-script.py console --matplotlib"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   ;; python-shell-completion-setup-code
   ;; "from IPython.core.completerlib import module_completion"
   ;; python-shell-completion-module-string-code
   ;; "';'.join(module_completion('''%s'''))\n"
   ;; python-shell-completion-string-code
   ;; "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
   ))

;; (defun ipython-autoreload ()
;;   (interactive)
;;   (python-shell-send-string "%load_ext autoreload")
;;   (python-shell-send-string "%autoreload 2"))

(defun ipython-ask-input ()
  "Prompt string to send to ipython shell."
  (interactive)
  (let* ((inp (read-from-minibuffer "[In]:")))
    (python-shell-send-string inp)))

(defun ipython-help ()
  (interactive)
  (python-shell-send-string
   (concat (python-object-at-point) "?"))) 

(defun ipython-pdb ()
  (interactive)
  (let ((cmd (format "python -m pdb %s"
		     (buffer-file-name))))
    (pdb cmd)))

(defun ipython-timeit-expression ()
  (interactive)
  (let ((expr (python-get-expression)))
    (python-shell-send-string
     (concat "%timeit " expr))))

(defun ipython-debug-expression (
  (interactive)
  (let ((expr (python-get-expression)))
    (python-shell-send-string
     (concat "%debug " expr)))))

(defun ipython-doc-buffer ()
  (interactive)
  (let ((expr (python-object-at-point)))
    ;; (add-hook 'temp-buffer-show-hook 'highlight-paren-mode)
    ;; (add-hook 'temp-buffer-show-hook 'rainbow-delimiters-mode)
    (with-output-to-temp-buffer "*ipython-documentation*"
      (princ
       (python-shell-send-string-no-output
	(concat expr "?"))))))

(defun ipython-set-current-directory ()
  (interactive)
  (python-shell-send-string-no-output
   (format "cd %s" (file-name-directory (buffer-file-name)))))

(defun ipython-send-current-file ()
  (interactive)
  (save-buffer)
  (ipython-set-current-directory)
  (python-shell-send-file (buffer-file-name)))

(defun ipython-dired ()
  (interactive)
  (let ((bf (buffer-file-name)))
    (python-shell-send-string
     (format "\"%s %s\""
	     (file-name-directory bf) bf))))

(defun python-define-my-keys ()
  "Hints for argument list of prepared function, which is not utilized by auto-complete"
  (define-keys
    (list python-mode-map
    	  inferior-python-mode-map)
    (list "C-c"		nil 
	  "C-c C-b"   	'python-shell-send-buffer
	  "C-c C-f"   	'python-shell-send-file
	  "C-c C-s"   	'python-shell-send-string
	  "C-c C-r"   	'python-shell-send-region
	  "C-c C-e"	'python-send-expression
	  "C-x C-e"	'python-send-expression
	  "C-c C-l"	'ipython-send-current-file
	  "C-c C-k"	'ipython-send-current-file
	  "C-c l"	'python-send-line
	  "C-c j"	'python-send-line-and-newline
	  "C-c C-j"	'python-send-line-and-newline
	  "C-x p"	'python-open-shell-other-window
	  ;; Following are based on ipython magic commands.
	  "C-c h"	'ipython-help
	  "C-c q"	'(lambda () (interactive)
			'  (python-shell-send-string "q"))
	  "C-c <RET>"	'(lambda () (interactive)
			'  (python-shell-send-string "\n"))
	  "C-c C-t"	'ipython-timeit-expression
	  "C-c C-p"	'ipython-pdb
	  "C-c C-d"	'ipython-debug-expression
	  "C-c <escape>"	'python-shell-switch-to-shell)))

;; (python-define-my-keys)
(use-ipython)

(add-hook 'python-mode-hook #'python-define-my-keys)
(add-hook 'inferior-python-mode-hook #'python-define-my-keys)

(setenv "PYTHONPATH" "C:/Code/python")

(global-set-key (kbd "<apps> <apps> p") 'run-python)
