;;; Folder for my configuration files.
(defvar *my-config-dir* "~/Documents/GitHub/EmacsConfig/")

;;; Settings for user interfaces.
(put 'narrow-to-region 'disabled nil)
(setq inhibit-splash-screen t) 
(setq find-function-C-source-directory "C:/Tools/emacs/src")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; (require 'cl)
;; (require 'evil)
(evil-mode 1)
(require 'pretty-lambdada)
(require 'rainbow-delimiters)

;; `paredit' functionalities are not suitable for languages other than lisp.
(dolist (h '(lisp-mode-hook
	     scheme-mode-hook
	     geiser-mode-hook
	     emacs-lisp-mode-hook
	     inferior-emacs-lisp-mode-hook
	     clojure-mode))
  (add-hook h 'paredit-mode))

;;; General minor modes
(dolist (h '(lisp-mode-hook
	     emacs-lisp-mode-hook
	     inferior-emacs-lisp-mode-hook
	     geiser-mode-hook
	     geiser-repl-mode-hook
	     clojure-mode
	     python-mode-hook
	     inferior-python-mode-hook
	     sml-mode-hook
	     inferior-sml-mode-hook
	     haskell-mode-hook
	     ess-mode-hook))
  (dolist (f '(show-paren-mode
	       turn-on-pretty-lambda-mode
	       electric-pair-mode
	       toggle-truncate-lines
	       rainbow-delimiters-mode))
    (add-hook h f)))

; Backup settings.
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.s"))
 delete-old-versions t
 kept-new-versions 3
 kept-old-versions 2
 version-control t)

(defun load-my-config (fname)
  (load-file (concat *my-config-dir* fname)))

;;; Enhanced key-bindings settings.
(load-my-config "more-keys.elc")
;;; Enhanced evil. Unuseful since emacs-mode can be activated by C-z
;(load-my-config "more-evil.el")

;;; Default language-specific loadings.
;; (load-my-config "more-lisp.el")
;; (load-my-config "more-scheme.el")
(load-my-config "more-python.elc")
(load-my-config "more-haskell.el")

;;; Use (<apps> <apps> r) to load R-mode(ESS)
;;; This is seperated loaded to save start-up time.
(load-my-config "more-r.el")	

;;; Use (<apps> <apps> m) to load sml-mode
(load-my-config "more-sml.el")

(setq default-directory "~/OneDrive/Code/")

(add-hook 'org-mode-hook
	  '(lambda () (require 'org)
	     (setq org-format-latex-options
		   (plist-put org-format-latex-options :scale 1.5))))
