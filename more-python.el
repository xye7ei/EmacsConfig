(require 'python)

;; Using the pre-defined functions within module `python.el'
;; python-util-strip-string
;; python-util-goto-line
;; python-util-forward-comment

(defvar --my-python-help-buffer-name- "*python-help-documentation*")
(defvar --my-python-output-buffer-name- "*python-output*")

(defun my-python-switch-to-shell-other-window ()
  "Switch other window to python shell buffer. "
  (interactive)
  (switch-to-buffer-other-window "*Python*")
  (other-window -1))

(defun --my-python-object-at-point ()
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
		 (progn (skip-syntax-backward "()")  ; go back through parens
			(skip-syntax-backward "w_.") ; go back through object syntax
			(skip-syntax-forward ".")    ; if looking at ., jump it
			(point))
		 (progn (skip-syntax-forward "w_.")
			(skip-syntax-backward ".")
			(point)))))
      rslt)))

(defun --my-python-rvalue-in-statement (evalstr)
  "Extract the RHS-value of evalstr if it is a statement.
Trick: non-quoted equal sign cannot appear in LHS of a statement. 
So match LHS with repeated Non-quote/Non-'='-symbol (such symbol
sequence should be a legal identifier, ensuring found '=' sign
is not nested in quotion) and a following '=' symbol. "
  (python-util-strip-string (replace-regexp-in-string "^[^\\'\\\"=]+ *=" "" evalstr)))

(defun --my-python-syntax-object-at-point ()
  "Find a executable statement at point. Jump out of string/paren at first
if position is nested in string/paren. "
  (save-excursion
    (let* ((ctp (python-syntax-context-type))
	   (pos (if ctp
		    (python-syntax-context ctp)
		  (point))))
      (goto-char pos)
      (python-nav-backward-statement)
      (python-nav-forward-statement)
      (let* ((beg (point))
	     (end (progn (python-nav-forward-statement)
			 (- (point) 1)))
	     (obj (buffer-substring-no-properties beg end)))
	obj))))

(defun --my-python-expression-at-point ()
  (save-excursion
    (let ((obj (--my-python-syntax-object-at-point)))
      (--my-python-rvalue-in-statement obj))))

(defun --my-stripped-line ()
  "Current edited line with white spaces from both sides stripped. "
  (save-excursion
    (buffer-substring-no-properties
     (progn (beginning-of-line)
	    (while (looking-at "[\s#]") (forward-char))
	    (point))
     (progn (end-of-line)
	    (point)))))

(defun my-python-send-line ()
  "Send current line to python shell. "
  (interactive)
  (save-excursion
    (python-shell-send-string (--my-stripped-line))))

(defun my-python-send-object ()
  "Send python expression at point to python shell. "
  (interactive)
  (let ((expr (--my-python-object-at-point)))
    (python-shell-send-string expr)))

(defun my-python-send-expression ()
  "Send python expression at point to python shell. "
  (interactive)
  (let ((expr (--my-python-expression-at-point)))
    (python-shell-send-string expr)))

(defun my-python-eval-expression-at-point-output-buffer ()
  "Eval expression at point and show the result in buffer. "
  (interactive)
  (save-excursion
    (let* ((expr (python-util-strip-string (--my-python-expression-at-point)))
	   (rslt (python-shell-send-string-no-output expr)))
      (let ((src (current-buffer))
	    (tar (get-buffer-create --my-python-output-buffer-name-)))
	(switch-to-buffer-other-window tar)
	(insert (format "eval(\"%s\") \n  ->\n%s" expr rslt))))))

(defun --my-python-insert-commented-line-at-point (x)
  (insert (format "# %s\n" x)))

(defun my-python-eval-print-expression-at-point ()
  "Eval expression at point and insert the result
as documented string into next new line. "
  (interactive)
  (let* ((expr (--my-python-expression-at-point))
	 (rslt (python-shell-send-string-no-output expr))
	 (rlns (split-string rslt "\n" t)))
    (end-of-line)
    (newline)
    (if (< (length rlns) 50)
	(while rlns
	  (--my-python-insert-commented-line-at-point (pop rlns)))
      (progn
	(dotimes (i 30)
	  (--my-python-insert-commented-line-at-point (pop rlns)))
	(insert "# ...\n")
	(while (> (length rlns) 20)
	  (pop rlns))
	(while rlns
	  (--my-python-insert-commented-line-at-point (pop rlns)))))))

(defun my-python-eval-message-expression-at-point ()
  "Show evaluation result. If `popup' is installed then use it
to hint, otherwise use built-in `message' method. "
  (interactive)
  (let* ((expr (--my-python-expression-at-point))
	 (rslt (python-shell-send-string-no-output expr)))
    (if (package-installed-p 'popup)
	(progn
	  (when (not (fboundp 'popup-tip))
	    (require 'popup))
	  (popup-tip rslt))
      (message rslt))))

(defun my-python-send-line-and-newline ()
  "Send python line at point to python shell.
This allows editing with interpreting on-the-fly! "
  (interactive)
  (my-python-send-line)
  (move-end-of-line 1)
  (newline))

(defun my-python-send-paragraph ()
  (interactive)
  (backward-paragraph)
  (let ((a (point)))
    (forward-paragraph)
    (python-shell-send-string
     (buffer-substring-no-properties a (point)))))

(defun my-python-send-all-above ()
  (interactive)
  (save-excursion
    ;; Wether to skip current block? How? 
    (python-shell-send-string
     (buffer-substring-no-properties 1 (line-end-position)))))


(defun my-python-pdb ()
  "Call pdb mode for current python file for debugging. "
  (interactive)
  (let ((cmd (format "python -m pdb %s"
		     (buffer-file-name))))
    (pdb cmd)))

(defun my-python-help-buffer ()
  "NOT COMPLETED - show ipython help document of python-object in temporary buffer. "
  (interactive)
  (let ((expr (--my-python-object-at-point)))
    (when (> (length expr) 0)		; Guard empty string
      (with-output-to-temp-buffer --my-python-help-buffer-name-
	(princ
	 (python-shell-send-string-no-output
	  (format "
try:
    help(%s)
except: 
    print('No help information available.')
"
		  expr)))))))

(defun my-python-run-file-in-os ()
  (interactive)
  (async-shell-command (format "python %s" (buffer-file-name))))



;; The following functions are based upon IPython's magical functionalities. 
;; They're not available when using naive CPython interpreter. 
(defun my-ipython-autoreload ()
  (interactive)
  (python-shell-send-string "%load_ext autoreload")
  (python-shell-send-string "autoreload 2"))

(defun my-ipython-ask-input ()
  "Prompt string to send to ipython shell. "
  (interactive)
  (let* ((inp (read-from-minibuffer "[In]:")))
    (python-shell-send-string inp)))

(defun my-ipython-timeit-expression ()
  "%timeit the python expression at point. "
  (interactive)
  (let ((expr (--my-python-expression-at-point)))
    (python-shell-send-string
     (concat "%timeit " expr))))

(defun my-ipython-debug-expression ()
  "%debug the python expression at point. "
  (interactive)
  (let ((expr (--my-python-expression-at-point)))
    (python-shell-send-string
     (concat "%debug " expr))))

(defun my-ipython-set-current-directory ()
  "Set ipython-shell's working directory to the currently edited
file's directory so that 'import' can work correctly. "
  (interactive)
  (python-shell-send-string-no-output
   (format "cd %s" (file-name-directory (buffer-file-name)))))

(defun my-ipython-send-current-file ()
  "Send currently edited file to ipython-shell as well as update
the working directory. "
  (interactive)
  (save-buffer)
  (my-ipython-set-current-directory)
  ;; (python-shell-send-file (buffer-file-name))
  (python-shell-send-string (concat "%run " (buffer-file-name))))

(defun my-ipython-dired ()
  "Show ipython-shell's working directory. "
  (interactive)
  (let ((bf (buffer-file-name)))
    (python-shell-send-string
     (format "\"%s %s\""
	     (file-name-directory bf) bf))))


;; Use IPython as the default intepreter when accessing python mode. 
(defun my-use-ipython ()
  "Assign ipython's working setups according to ipython official website. "
  (when (executable-find "ipython")
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args ""; "-i C:/Python35/Scripts/ipython-script.py console --matplotlib"
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     ;; Supporting auto-completion in emacs even when PyReadline not available in windows. 
     ;; python-shell-completion-setup-code
     ;; "from IPython.core.completerlib import module_completion"
     ;; python-shell-completion-string-code
     ;; "';'.join(module_completion('''%s'''))\n"
     ;; python-shell-completion-string-code
     ;; "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
     ))
  )


;; Define my extended custom keys.
;; Mind some of them are only dependent upon naive CPython, but some are
;; dependent upon IPython,
(defun python-define-my-keys ()
  (let ((pkms '(;; "C-c"		nil 
		;; Some are already covered in `python.el'
		;; "C-c C-b"   	python-shell-send-buffer
		;; "C-c C-f"   	python-shell-send-file
		;; "C-c C-s"   	python-shell-send-string
		;; "C-c C-r"   	python-shell-send-region
		"C-c e"		my-python-send-object
		"C-c M-h"	my-python-send-paragraph
		"C-c l"		my-python-send-line
		"C-M-j"		my-python-send-line-and-newline
		"C-M-,"		my-python-eval-print-expression-at-point
		"C-M-."		my-python-eval-message-expression-at-point
		"C-M-;"		my-python-eval-expression-at-point-output-buffer
		"C-c C-a"	my-python-send-all-above
		"C-c z"		my-python-switch-to-shell-other-window
		"C-c M-z"	my-python-switch-to-shell-other-window
		"C-c <tab>"	my-python-help-buffer 
		"C-c !"		my-python-run-file-in-os
		"C-c C-p"	my-python-pdb
		;; Following are based on ipython magic commands.
		;; "C-c C-l"	my-ipython-send-current-file
		"C-c M-a"	my-ipython-autoreload
		"C-c C-k"	my-ipython-send-current-file
		;; "C-c q"		(lambda () (interactive) (python-shell-send-string "q"))
		;; "C-c <RET>"	(lambda () (interactive) (python-shell-send-string "\n"))
		"C-c C-t"	my-ipython-timeit-expression
		"C-c C-d"	my-ipython-debug-expression
		"C-c <escape>"	python-shell-switch-to-shell)))
    (while pkms
      (let* ((k (pop pkms))
	     (m (pop pkms))) 
	(define-key python-mode-map (kbd k) m)))))

(add-hook 'python-mode-hook 'my-use-ipython) 
(add-hook 'python-mode-hook 'python-define-my-keys)

(global-set-key (kbd "<apps> <apps> p") 'run-python)
