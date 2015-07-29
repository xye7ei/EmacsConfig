(defun haskell-define-my-keys ()
  (define-keys
    (list haskell-mode-map
	  haskell-cabal-mode-map)
    (list "C-c C-l"	'haskell-process-load-or-reload
	  "C-c C-z"	'haskell-interactive-switch
	  "C-c C-n C-t"	'haskell-process-do-type
	  "C-c C-n C-i"	'haskell-process-do-info
	  "C-c C-n C-c"	'haskell-process-cabal-build
	  "C-c C-n c"	'haskell-process-cabal
	  "SPC"		'haskell-mode-contextual-space
	  "C-c C-k"	'haskell-compile
	  "C-c C-o"	'haskell-compile
	  "C-c C-r"	'haskell-process-restart)))

(defun load-haskell ()
  (interactive)
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
