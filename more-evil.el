;;; These are enhanced key functionalities which dependend upon
;;; evil functions. 

(defun scroll-other-some ()
  (interactive)
  (scroll-other-window -3))
(defun scroll-other-some-down ()
  (interactive)
  (scroll-other-window 3))
(defun up-part-page ()
  (interactive)
  (evil-scroll-line-up 3))
(defun down-part-page ()
  (interactive)
  (evil-scroll-line-down 3))
(defun find-ref-word ()
  (interactive)
  (let ((w (concat "\\<"
		   (buffer-substring-no-properties
		    (progn (skip-syntax-backward "w_") (point))
		    (progn (skip-syntax-forward "w_") (point)))
		   "\\>")))
    (evil-search-word t nil w)))
(defun find-ref-word-backward ()
  (interactive)
  (let ((w (concat "\\<"
		   (buffer-substring-no-properties
		    (progn (skip-syntax-backward "w_") (point))
		    (progn (skip-syntax-forward "w_") (point)))
		   "\\>")))
    (evil-search-word nil nil w))) 

(defun more-evil ()
  "Custom more evil keys. " 
  (define-keys
    (list evil-insert-state-map
	  evil-visual-state-map
	  evil-normal-state-map
	  evil-motion-state-map)
    (list "C-w"		'kill-buffer
	  "C-y" 	'yank
	  "C-a" 	'evil-beginning-of-line
	  "C-e" 	'end-of-line ; Better than `evil-end-of-line'.
	  "C-d"		'delete-char
	  "C-n" 	'evil-next-line
	  "C-p" 	'evil-previous-line
	  "C-0"     	'evil-window-mru
	  "C-1"     	'evil-window-top-left
	  "C-2"     	'evil-window-right
	  "C-3"     	'evil-window-down
	  "C-4"     	'evil-window-bottom-right
	  "C-`"		'evil-window-mru
	  ))
  (define-keys
    (list evil-visual-state-map		; except insert mode
	  evil-normal-state-map
	  evil-motion-state-map)
    (list "z z" 	'recenter-top-bottom
	  "z j" 	'evil-scroll-line-up
	  "z k" 	'evil-scroll-line-down
	  "z h" 	'evil-scroll-line-left
	  "z l" 	'evil-scroll-line-right
	  "z p" 	'evil-scroll-page-up
	  "z n" 	'evil-scroll-page-down
	  "z v" 	'scroll-other-window
	  "z V" 	'scroll-other-window-down
	  "g c" 	'goto-char
	  "z w" 	'scroll-other-some-down
	  "z W" 	'scroll-other-some-down
	  "z s" 	'scroll-other-some
	  "z S" 	'scroll-other-some
	  "z d" 	'up-part-page
	  "z D" 	'up-part-page
	  "z e" 	'down-part-page
	  "z E" 	'down-part-page
	  "C-*" 	'find-ref-word
	  "C-#" 	'find-ref-word-backward
	  "<C-up>" 	'evil-scroll-line-up
	  "<C-down>"	'evil-scroll-line-down
	  "<C-left>"	'backward-sexp
	  "<C-right>"	'forward-sexp)))

(more-evil)
