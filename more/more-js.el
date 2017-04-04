;; Communicate with node-js process.
;;; Require js3-mode
(require 'js3-mode)

(defconst -node-process-name- "*nodejs*")

;;; Utilities
(defun my-node-get-process ()
  (get-buffer-process -node-process-name-))

(defun my-node-hint-region (start end)
  (interactive "r")
  (message (buffer-substring-no-properties start end)))

(defun my-node-node-string-at-point ()
  (js3-node-string (js3-node-at-point)))

(defun my-node-node-string-enclosing-node (start end)
  (interactive "r")
  (js3-node-string (js3-mode-find-enclosing-node start end)))

;;; Communication
(defun my-node-process-send-string (str)
  (let ((proc (my-node-get-process)))
    (if proc
	(progn
	  (comint-send-string proc str)
	  (if (neq (substring str (- (length str) 1))
	  	   "\n")
	      (comint-send-string proc "\n"))
	  (let ((str (replace-regexp-in-string "[\t\n ]+" " " str)))
	    (message (format "\"%s\" sent."
			     (if (< (length str) 50)
				 str
			       (format "%s ..." (substring str 0 50)))))))
      (message "You should start `node-repl' first. "))))

(defun my-node-process-send-region (start end)
  (interactive "r")
  (save-excursion
    (my-node-process-send-string
     (buffer-substring-no-properties start end))))

(defun my-node-process-send-line ()
  (interactive)
  (let ((ln (buffer-substring-no-properties
	     (line-beginning-position)
	     (line-end-position))))
    (my-node-process-send-string ln)))

(defun my-node-process-send-node ()
  (interactive)
  (let ((nd-str (js3-node-string (js3-node-at-point))))
    (my-node-process-send-string nd-str)))

(defun my-node-process-send-enclosing-node (start end)
  (interactive "r")
  (let ((nd-str (my-node-node-string-enclosing-node start end)))
    (my-node-process-send-string nd-str)))

(defun my-node-process-send-defun ()
  (interactive)
  (let ((fn (js3-mode-function-at-point)))
    (if fn
	(let ((nd-str (js3-node-string fn)))
	  (my-node-process-send-string nd-str))
      (message "No function node at point found."))))

(defun my-node-process-send-paragraph ()
  (interactive)
  (save-excursion
    (let ((pgf (buffer-substring-no-properties
		(progn (backward-paragraph) (point))
		(progn (forward-paragraph) (point))))) 
      (my-node-process-send-string pgf))))

(defun my-node-map-keys ()
  (define-key js3-mode-map (kbd "C-c l") 'my-node-process-send-line)
  (define-key js3-mode-map (kbd "C-x C-e") 'my-node-process-send-node)
  (define-key js3-mode-map (kbd "C-c C-b") 'my-node-process-send-enclosing-node)
  (define-key js3-mode-map (kbd "C-c C-r") 'my-node-process-send-region)
  (define-key js3-mode-map (kbd "C-M-x") 'my-node-process-send-defun)
  (define-key js3-mode-map (kbd "C-M-x") 'my-node-process-send-paragraph))

(add-hook 'js3-mode-hook 'my-node-map-keys)
