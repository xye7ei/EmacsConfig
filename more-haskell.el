
(defun haskell-define-my-keys ()
  (let ((kms (list "C-c C-l"	'haskell-process-load-or-reload
		   "C-c C-z"	'haskell-interactive-switch
		   "C-c C-n C-t"	'haskell-process-do-type
		   "C-c C-n C-i"	'haskell-process-do-info
		   "C-c C-n C-c"	'haskell-process-cabal-build
		   "C-c C-n c"	'haskell-process-cabal
		   "SPC"		'haskell-mode-contextual-space
		   "C-c C-k"	'haskell-compile
		   "C-c C-o"	'haskell-compile
		   "C-c C-r"	'haskell-process-restart)))
    (while kms
      (let ((k (pop kms))
	    (f (pop kms)))
	(define-key haskell-mode-map (kbd k) f)
	(define-key haskell-cabal-mode-map (kbd k) f)))))

(defun load-haskell ()
  (interactive)
  (when (not (package-installed-p 'haskell-mode))
    (package-refresh-contents)
    (package-install 'haskell-mode))
  (require 'haskell) 
  (add-hooks (list 'haskell-mode-hook)
	     (list 'interactive-haskell-mode
		   'turn-on-haskell-indentation)) 
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-type 'cabal-repl))
  (add-hook 'haskell-mode-hook 'haskell-define-my-keys))

(global-set-key (kbd "<apps> <apps> h") 'load-haskell)
