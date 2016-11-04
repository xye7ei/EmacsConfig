(require 'python)

(defconst my-python--help-buffer-name
  "*Python-help*"
  "The temp-buffer name for displaying Python documentation info. ")

(defconst my-python--shell-setup-code
  "from pprint import pprint as __pprint
import sys as __sys

__ORIG_DISP_HOOK = __sys.displayhook

def __pp_hook(value):
    if None is not value:
        __builtins__._ = value
        __pprint(value)

__sys.displayhook = __pp_hook

from timeit import timeit as __timeit
"
  "The code that hooks `pprint' as replacement of standard
  `print' as output method.")

(defun my-python--stripped-line ()
  "Current edited line with white spaces from both sides stripped. "
  (save-excursion
    (buffer-substring-no-properties
     (progn (beginning-of-line)
            (while (looking-at "[\s#]") (forward-char))
            (point))
     (progn (end-of-line)
            (point)))))

(defun my-python-shell-send-line ()
  "Send current line to python shell. "
  (interactive)
  (save-excursion
    (python-shell-send-string (my-python--stripped-line))))

(defun my-python-pdb ()
  "Call pdb mode for current python file for debugging. "
  (interactive)
  (let ((cmd (format "python -m pdb %s"
                     (buffer-file-name))))
    (pdb cmd)))

(defun my-python-run-compilation ()
  "Call Python to run current file with `compilation-mode'. "
  (interactive)
  (compile (format "python %s" (buffer-file-name))))

(defun my-python-run-compilation-with-version (arg)
  "Call Python to run current file with `compilation-mode'. "
  (interactive "P")
  (compile (format "py -%s %s" arg (buffer-file-name))))

(defun my-python-shell-exec-file ()
  "Send current file to Python shell buffer. "
  (interactive)
  (python-shell-send-file (buffer-file-name)))

(defun my-python-switch-to-shell-other-window ()
  "Switch another window to Python shell buffer. "
  (interactive)
  (save-excursion
    (python-shell-switch-to-shell)
    (other-window 1)))

(defun my-python-print-help ()
  "Show help information of Python symbol in a temporary buffer."
  (interactive)
  (save-excursion
    (let ((symb (python-info-current-symbol nil)))
      (with-output-to-temp-buffer my-python--help-buffer-name
	(princ
	 (python-shell-send-string-no-output
	  (format "
try:
    exec('help(%s)')
except:
    print('No help information available.')
"
		  symb)))))))

(defun my-python-toggle-ipython ()
  (interactive)
  (if (equal python-shell-interpreter "python")
      (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter "python")))

;; Problem by integration of Python module `pyreadline'
;; https://github.com/gregsexton/ob-ipython/issues/28
;; This issue raises possibly in Emacs > 25.0.
(setq python-shell-completion-native-enable nil)

(custom-set-variables
 `(python-shell-interpreter-args
   ,(format "-i -c \"%s\"" my-python--shell-setup-code)))

(cond
 ((fboundp 'jedi:setup) (add-hook 'python-mode-hook 'jedi:setup))
 ((fboundp 'anaconda-mode) (add-hook 'python-mode-hook 'anaconda-mode)))

(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "C-c l"  ) 'my-python-shell-send-line)
	    (define-key python-mode-map (kbd "C-c C-a") 'my-python-switch-to-shell-other-window)
	    (define-key python-mode-map (kbd "C-c C-h") 'my-python-print-help)
	    (define-key python-mode-map (kbd "C-c C-d") 'my-python-pdb)
	    (define-key python-mode-map (kbd "C-c C-k") 'my-python-run-compilation)
	    (define-key python-mode-map (kbd "C-c C-b") 'my-python-shell-exec-file)
	    (define-key python-mode-map (kbd "C-c C-m") 'my-python-run-compilation-with-version)
	    (define-key python-mode-map (kbd "C-c C-i") 'my-python-toggle-ipython)
	    ))

