;;; Folder for my configuration files.
(defconst *my-config-dir* "~/Documents/GitHub/EmacsConfig/")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

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

(defun load-my-config (fname)
  (load-file (concat *my-config-dir* fname)))

;;; Enhanced key-bindings settings.
(load-my-config "more-keys.el") 
;;; For TeX
(load-my-config "more-tex.el") 
;;; For languages.
(load-my-config "more-python.el")
(load-my-config "more-haskell.el") 
;;; For R
(load-my-config "more-r.el") 
(load-my-config "more-sml.el")


;;; Customizing working environment.
(defun my-copy-to-drive-active ()
  (interactive)
  (copy-file (buffer-file-name)
	     (format "~/OneDrive/Active/%s" (buffer-name))))
(defun my-delete-from-drive-active ()
  (interactive)
  (delete-file (format "~/OneDrive/Active/%s" (buffer-name))))

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
(setq default-directory "~/OneDrive/") 
(set-language-environment "UTF-8")
