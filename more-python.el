(require 'python)

(defun python-open-shell-other-window ()
  "Switch other window to python shell buffer. "
  (interactive)
  (switch-to-buffer-other-window "*Python*")
  (other-window -1))

(defun python-object-at-point ()
  "Fetch the python object at editing point.

*. Examples (with bar symbol emulating the caret):

    random.randint((|34 + 28), 5678)  			==>> random.randint
    my_modu.my_func(her_modu.her_func(123|45, 67  	==>> 12345
    @a_decorator(|di, du) 				==>> a_decorator
    my_modu.my_sub_modu.|my_func			==>> my_modu.my_sub_modu.my_func

*. Needs further tuning to work robustly.
"
  (save-excursion
    (let ((rslt (buffer-substring-no-properties
		 (progn (while (looking-back "[\(\)\.]") (backward-char))
			(while (looking-back "[^ #@\(\)]") (backward-char))
			(point))
		 (progn (skip-syntax-forward "w_.")
			(skip-syntax-backward ".")
			(point)))))
      rslt)))

(defun python-expression-at-point ()
  "Fetch the python expression at editing point, i.e.
1. Right hand side of some assignment statement;
2. If not at assignment statement, then the whole line. "
  (save-excursion
    (progn
      (while (not (looking-back "[=\n#]")) (backward-char))
      (when (looking-at "\s")
	(forward-char 1))
      (buffer-substring-no-properties
       (point)
       (line-end-position)))))

(defun stripped-line ()
  "Current edited line with white spaces from both sides stripped. "
  (save-excursion
    (buffer-substring-no-properties
     (progn (beginning-of-line)
	    (while (looking-at "[\s#]") (forward-char))
	    (point))
     (progn (end-of-line)
	    (point)))))

(defun python-send-line ()
  "Send current line to python shell. "
  (interactive)
  (save-excursion
    (python-shell-send-string (stripped-line))))

(defun python-send-expression ()
  "Send python expression at point to python shell. "
  (interactive)
  (let ((expr (python-expression-at-point)))
    (python-shell-send-string expr)))

(defun python-send-line-and-newline ()
  "Send python line at point to python shell.
This allows editing with interpreting on-the-fly! "
  (interactive)
  (python-send-line)
  (newline))

(defun python-send-paragraph ()
  (interactive)
  (backward-paragraph)
  (let ((a (point)))
    (forward-paragraph)
    (python-send-string
     (python-shell-send-string-no-output (buffer-substring-no-properties a (point))))))

(defun use-ipython ()
  "Assign ipython's working setups according to ipython official website. "
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""; "-i C:/Tools/Python34/Scripts/ipython-script.py console --matplotlib"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   ;; Supporting auto-completion in emacs even when PyReadline not available in windows. 
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
   ))

;; (defun ipython-autoreload ()
;;   (interactive)
;;   (python-shell-send-string "%load_ext autoreload")
;;   (python-shell-send-string "%autoreload 2"))

(defun ipython-ask-input ()
  "Prompt string to send to ipython shell. "
  (interactive)
  (let* ((inp (read-from-minibuffer "[In]:")))
    (python-shell-send-string inp)))

(defun ipython-help ()
  "Send object-at-point appended with '?' to print help document in ipython shell. "
  (interactive)
  (python-shell-send-string
   (concat (python-object-at-point) "?"))) 

(defun ipython-pdb ()
  "Call pdb mode for current python file for debugging. "
  (interactive)
  (let ((cmd (format "python -m pdb %s"
		     (buffer-file-name))))
    (pdb cmd)))

(defun ipython-timeit-expression ()
  "%timeit the python expression at point. "
  (interactive)
  (let ((expr (python-expression-at-point)))
    (python-shell-send-string
     (concat "%timeit " expr))))

(defun ipython-debug-expression ()
  "%debug the python expression at point. "
  (interactive)
  (let ((expr (python-expression-at-point)))
    (python-shell-send-string
     (concat "%debug " expr))))

(defun python-doc-buffer ()
  "NOT COMPLETED - show ipython help document of python-object in temporary buffer. "
  (interactive)
  (let ((expr (python-object-at-point)))
    (with-output-to-temp-buffer "*ipython-documentation*"
      (princ
       (python-shell-send-string-no-output
	(format "help(%s)" expr))))))

(defun ipython-set-current-directory ()
  "Set ipython-shell's working directory to currently edited file's directory
so that 'import' can work correctly. "
  (interactive)
  (python-shell-send-string-no-output
   (format "cd %s" (file-name-directory (buffer-file-name)))))

(defun ipython-send-current-file ()
  "Send currently edited file to ipython-shell as well as update
the working directory. "
  (interactive)
  (save-buffer)
  (ipython-set-current-directory)
  (python-shell-send-file (buffer-file-name)))

(defun ipython-dired ()
  "Show ipython-shell's working directory. "
  (interactive)
  (let ((bf (buffer-file-name)))
    (python-shell-send-string
     (format "\"%s %s\""
	     (file-name-directory bf) bf))))

(defun python-define-my-keys ()
  (let ((pkms '("C-c"		nil 
		"C-c C-b"   	python-shell-send-buffer
		"C-c C-f"   	python-shell-send-file
		"C-c C-s"   	python-shell-send-string
		"C-c C-r"   	python-shell-send-region
		"C-c C-e"	python-send-expression
		"C-x C-e"	python-send-expression
		"C-c M-h"	python-send-paragraph
		"C-c l"		python-send-line
		"C-c j"		python-send-line-and-newline
		"C-M-j"		python-send-line-and-newline
		"C-c b"		python-send-paragraph
		"C-x C-p"	python-open-shell-other-window
		"C-c <tab>"	python-doc-buffer 
		;; Following are based on ipython magic commands.
		"C-c C-l"	ipython-send-current-file
		"C-c C-k"	ipython-send-current-file
		"C-c C-h"	ipython-help
		"C-c q"		(lambda () (interactive) (python-shell-send-string "q"))
		"C-c <RET>"	(lambda () (interactive) (python-shell-send-string "\n"))
		"C-c C-t"	ipython-timeit-expression
		"C-c C-p"	ipython-pdb
		"C-c C-d"	ipython-debug-expression
		"C-c <escape>"	python-shell-switch-to-shell)))
    (while pkms
      (let* ((k (pop pkms))
	     (m (pop pkms))) 
	(define-key python-mode-map (kbd k) m)))))

(use-ipython)

(add-hook 'python-mode-hook 'python-define-my-keys)
;; (add-hook 'inferior-python-mode-hook #'python-define-my-keys)

(global-set-key (kbd "<apps> <apps> p") 'run-python)
