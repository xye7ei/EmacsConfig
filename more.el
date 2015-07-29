;;; Settings for user interfaces.
(put 'narrow-to-region 'disabled nil)
(setq inhibit-splash-screen t) 
(setq find-function-C-source-directory "C:/Tools/emacs/src")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(require 'cl)
(require 'evil) (evil-mode 1)
(require 'pretty-lambdada)
(require 'rainbow-delimiters)

(defun add-hooks (hooks funcs)
  (dolist (h hooks)			; Mind the quoting ,hooks
    (dolist (f funcs)
      (add-hook h f))))

(defun 2-part-list (lst)
  (and lst
       (cons (list (car lst) (cadr lst))
	     (2-part-list (cddr lst)))))

(defun define-keys (mode-maps key-funcs)
  " `mode-maps' is a list of mode-maps;
`key-funcs' is a flat list of key-string and quoted-functions;"
  (dolist (k-f (2-part-list key-funcs))
    (dolist (m mode-maps)
      (define-key m (kbd (first k-f)) (second k-f)))))

;; `paredit' functionalities is not suitable for other languages.
(add-hooks (list 'lisp-mode-hook
		 'scheme-mode-hook
		 'geiser-mode-hook
		 'emacs-lisp-mode-hook
		 'inferior-emacs-lisp-mode-hook
		 'clojure-mode)
	   (list 'paredit-mode))

;;; general modes
(add-hooks (list 'lisp-mode-hook
		 'emacs-lisp-mode-hook
		 'inferior-emacs-lisp-mode-hook
		 'geiser-mode-hook
		 'geiser-repl-mode-hook
		 'clojure-mode
		 'python-mode-hook
		 'inferior-python-mode-hook
		 'sml-mode-hook
		 'inferior-sml-mode-hook
		 'haskell-mode-hook
		 'ess-mode-hook)
	   (list 'show-paren-mode
		 'turn-on-pretty-lambda-mode
		 'electric-pair-mode
		 'toggle-truncate-lines
		 'rainbow-delimiters-mode))

; Backup settings.
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.s"))
 delete-old-versions t
 kept-new-versions 3
 kept-old-versions 2
 version-control t)

;;; Enhanced key-bindings settings.
(load-file "~/OneDrive/Code/more-keys.el")
(load-file "~/OneDrive/Code/more-evil.el")

;;; Default language-specific loadings.
(load-file "~/OneDrive/Code/more-lisp.el")
(load-file "~/OneDrive/Code/more-scheme.el")
(load-file "~/OneDrive/Code/more-python.el")
(load-file "~/OneDrive/Code/more-haskell.el")
;;; Use (<apps> <apps> r) to load R-mode(ESS)
;;; This is seperated loaded to save start-up time.
(load-file "~/OneDrive/Code/more-r.el")	
;;; Use (<apps> <apps> m) to load sml-mode
(load-file "~/OneDrive/Code/more-sml.el")

(setq default-directory "~/OneDrive/Code/")
