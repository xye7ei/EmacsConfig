(require 'python)

;; Using the pre-defined functions within module `python.el'
;; python-util-strip-string
;; python-util-goto-line
;; python-util-forward-comment

(defvar --my-python-help-buffer-name- "*python-help-documentation*")
(defvar --my-python-output-buffer-name- "*python-output*")

(defun --my-python-object-at-point ()
  "Fetch the python object at current point.

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
sequence should be a legal identifier, ensuring found '=' sign is
not nested in quotion since literal string cannot be LHS) and a
following '=' symbol. "
  (python-util-strip-string
   (replace-regexp-in-string
    "^[^\\'\\\"=]+ *="
    ""
    evalstr)))

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

(defun my-python-send-syntax-object-at-point ()
  "Send python expression at point to python shell. "
  (interactive)
  (let ((expr (--my-python-syntax-object-at-point)))
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
This allows editing with interpreting on-the-fly!

FIXME: What if point is in the middle of multi-line statement? "
  (interactive)
  (my-python-send-line)
  (move-end-of-line 1)
  (newline))

(defun my-python-send-paragraph ()
  "FIXME: Should be replaced by `xxx-send-statement', which is
more rigorous."
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
  "NOT COMPLETED - show Python help document of python-object in
temporary buffer.

FIXME: How to get the object-at-point?"
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

(defun my-python-compile-file ()
  (interactive)
  (compile (format "python %s" (buffer-file-name))))

(defun my-python-exec-file ()
  (interactive)
  (python-shell-send-file (buffer-file-name)))


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
     ;; python-shell-interpreter-args "" ; "-i C:/Python35/Scripts/ipython-script.py console --matplotlib"
     ;; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     ;; python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     ;; Supporting auto-completion in emacs even when PyReadline not available in windows. 
     ;; python-shell-completion-setup-code
     ;; "from IPython.core.completerlib import module_completion"
     ;; python-shell-completion-string-code
     ;; "';'.join(module_completion('''%s'''))\n"
     ;; python-shell-completion-string-code
     ;; "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
     ))
  )

(defun my-python-switch-to-shell-other-window ()
  (interactive)
  (save-excursion
    (python-shell-switch-to-shell)
    (other-window 1)))

(defun python-define-my-keys ()
  "Define my extended custom keys. Some of them are only
dependent upon naive CPython, but some are dependent upon
IPython, Some useful key-bnidings are already supplied by
`python.el' and can be queried in function `python-mode'. They
should have no conflicts with the key definitions below.
"

  (let ((pkms '(;; "C-c"		nil 
		"C-c e"		my-python-send-syntax-object-at-point
		"C-c h"		my-python-send-paragraph
		"C-c l"		my-python-send-line
		"C-c a"		my-python-send-all-above
		"C-c j"		my-python-send-line-and-newline
		;; "C-c ,"		my-python-eval-print-expression-at-point
		;; "C-c ."		my-python-eval-message-expression-at-point
		;; "C-c ;"		my-python-eval-expression-at-point-output-buffer
		;; 
		"C-c C-a"	my-python-switch-to-shell-other-window
		"C-c C-h"	my-python-help-buffer 
		"C-c C-d"	my-python-pdb
		"C-c C-k"	my-python-compile-file
		"C-c C-b"	my-python-exec-file
		;; Following are based on ipython "M"agic commands. So
		;; "M"eta key is used.
		;; "C-c M-a"	my-ipython-autoreload
		"C-c M-k"	my-ipython-send-current-file
		"C-c M-t"	my-ipython-timeit-expression
		"C-c M-d"	my-ipython-debug-expression
		)))
    (while pkms
      (let* ((k (pop pkms))
	     (m (pop pkms))) 
	(define-key python-mode-map (kbd k) m)))))

;; (add-hook 'python-mode-hook 'my-use-ipython) 
(add-hook 'python-mode-hook 'python-define-my-keys)
