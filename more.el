;;; Folder for my configuration files.
;;; (defconst *my-config-dir* "~/Documents/GitHub/EmacsConfig/")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (p '(evil
	     pretty-lambdada
	     rainbow-delimiters
	     paredit
	     ))
  (unless (package-installed-p p)
    (package-install p)))

(package-initialize)

;; (require 'pretty-lambdada)
;; (require 'rainbow-delimiters)

(defun my-general-modes ()
  (show-paren-mode)
  (pretty-lambda-mode)
  (turn-on-pretty-lambda-mode)
  (electric-pair-mode)
  (rainbow-delimiters-mode))

(add-hook 'lisp-mode-hook 			'my-general-modes)
(add-hook 'emacs-lisp-mode-hook 		'my-general-modes)
(add-hook 'inferior-emacs-lisp-mode-hook 	'my-general-modes)
(add-hook 'geiser-mode-hook 			'my-general-modes)
(add-hook 'geiser-repl-mode-hook 		'my-general-modes)
;; (add-hook 'clojure-mode-hook 			'my-general-modes)
(add-hook 'python-mode-hook 			'my-general-modes)
(add-hook 'inferior-python-mode-hook 		'my-general-modes)
(add-hook 'sml-mode-hook 			'my-general-modes)
(add-hook 'inferior-sml-mode-hook 		'my-general-modes)
;; (add-hook 'haskell-mode-hook 			'my-general-modes)
;; (add-hook 'ess-mode-hook 			'my-general-modes)

(defun my-lisp-modes ()
  (my-general-modes)
  ;; (require 'paredit)
  (paredit-mode))

;; `paredit' functionalities are not suitable for languages other than lisp.
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook              'my-lisp-modes)
(add-hook 'geiser-mode-hook              'my-lisp-modes)
(add-hook 'emacs-lisp-mode-hook          'my-lisp-modes)
(add-hook 'inferior-emacs-lisp-mode-hook 'my-lisp-modes)
;; (add-hook 'clojure-mode                  'my-lisp-modes)


(defun load-my-config (fname)
  (load-file (concat *my-config-dir* fname)))

;;; Enhanced key-bindings settings.
(load-my-config "more-keys.el") 
;;; For TeX
(load-my-config "more-tex.el") 
;;; For languages.
(load-my-config "more-python.el")
;; (load-my-config "more-haskell.el") 
;;; For R
; (load-my-config "more-r.el") 
;;; For sml
(load-my-config "more-sml.el")
;;; For scheme
;; (custom-set-variables '(scheme-program-name "petite"))
(custom-set-variables '(scheme-program-name "racket"))


;;; Customizing working environment.
(defun my-copy-to-drive-active ()
  (interactive)
  (copy-file (buffer-file-name)
	     (format "~/../../OneDrive/Active/%s" (buffer-name))))
(defun my-delete-from-drive-active ()
  (interactive)
  (delete-file (format "~/../../OneDrive/Active/%s" (buffer-name))))

;;; Backup settings.
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.s"))
 delete-old-versions t
 kept-new-versions 3
 kept-old-versions 2
 version-control t)

;;; Settings for user interfaces.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1) 
(put 'narrow-to-region 'disabled nil)
(setq inhibit-splash-screen t) 
(setq find-function-C-source-directory "C:/Tools/emacs/src")

;;; Use emacs state instead of evil-normal-state
(add-hook 'evil-mode-hook
	  '(lambda () (interactive)
	     (setq evil-normal-state-cursor '(box "#00E600"))))
(add-hook 'evil-emacs-state-entry-hook
	  '(lambda () (interactive)
	     (setq evil-emacs-state-cursor '((bar . 3) "black"))
	     (define-key evil-emacs-state-map (kbd "<ESC> [") 'evil-exit-emacs-state)))
(evil-mode 1)

;;; 
;; (setq default-directory "~/OneDrive/") 
(set-language-environment "UTF-8")
