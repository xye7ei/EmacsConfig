(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ;; ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
   ["#212526" "#99224b" "#80f000" "#fce94f" "#dddddd" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (adwaita)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 100 :width normal)))))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


(defmacro add-hooks (hooks funcs)
  `(dolist (h ',hooks)			; Mind the quoting ,hooks
     (dolist (f ',funcs)
   (add-hook h f))))

(defmacro define-key-more (mps &rest k-ms)
  "
`mps' is a list of free variables (here the mode-maps), which should not be quoted;
`k-ms' are a undelimited sequence of key-definition and unquoted function-name, like (\"KEY-1\" func-1 \"KEY-2\" func-2 ...)
"
  (defun 2-part-list (lst)
    (and lst
	 ;; (cons (cons (car lst) (cadr lst))
	 (cons (list (car lst) (cadr lst))
	       (2-part-list (cddr lst)))))
  `(dolist (m ,(cons 'list mps))
     (dolist (pr ',(2-part-list k-ms))
       (define-key m (kbd (first pr)) (second pr))))) ; m should not be quoted 

;; ;; Vi-emulation: evil
(require 'evil)
(evil-mode 1)
;; ;; For lisp general
(require 'pretty-lambdada)
(require 'rainbow-delimiters)

;; `paredit' functionalities is not suitable for other languages.
(add-hooks (lisp-mode-hook
	    emacs-lisp-mode-hook
	    inferior-emacs-lisp-mode-hook
	    clojure-mode)
	   (paredit-mode))		

(add-hooks (lisp-mode-hook
	    emacs-lisp-mode-hook
	    inferior-emacs-lisp-mode-hook
	    clojure-mode
	    python-mode-hook
	    inferior-python-mode-hook
	    haskell-mode)
	   (show-paren-mode
	    turn-on-pretty-lambda-mode
	    electric-pair-mode
	    toggle-truncate-lines
	    rainbow-delimiters-mode))

;; For python
;; Adapt cwd path for importing modules
(setenv "PYTHONPATH" "C:/Code/python")
;; (eval-after-load 'python-mode
;;   '(toggle-truncate-lines))

;; For common lisp
; (require 'slime)
(load (expand-file-name
	"~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program
	"C:/Tools/ccl/wx86cl.exe")

;; For scheme!
;; 1. Petite Scheme
; (push "~/.emacs.d/iuscheme" load-path)
; (require 'iuscheme)
; (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
; (autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
; (push '("//.ss" . scheme-mode) auto-mode-alist)
; (custom-set-variables '(scheme-program-name "petite"))
; (setq scheme-program-name 
; 	"C:/Tools/ChezSchemeVersion84/bin/ti3nt/petite.exe")

;; 2. Geiser
; (load-file "~/.emacs.d/geiser/geiser.el")
; (require 'geiser)
; (setq geiser-impl-installed-implementations '(racket))
; (setq geiser-racket-binary "C:/Program Files/Racket/Racket.exe")
; (require 'geiser-install)

; Backup settings.
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.s"))
 delete-old-versions t
 kept-new-versions 3
 kept-old-versions 2
 version-control t)

;; Custom key extension
(load-file "~/OneDrive/Code/more-keys.el")
(load-file "~/OneDrive/Code/more-python.el")
(load-file "~/OneDrive/Code/more-haskell.el")

(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-set-key (kbd "<apps>") 		'toggle-frame-maximized)
(global-set-key (kbd "S-<apps>") 	'toggle-frame-fullscreen) 
