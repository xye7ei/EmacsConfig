;; This file incorperate some fundamental extensive functions.

;; Package activation.
;; - Make it called-by-need to save startup performance.
(defun my-packages-activate ()
  (interactive)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))


;; Dark Theme 
(defun my-dark-theme ()
  (interactive)
  (custom-set-variables
   '(custom-enabled-themes (quote (deeper-blue))))
  (custom-set-faces
   '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 90 :width normal))))))

;; Light Theme 
(defun my-light-theme ()
  (interactive)
  (custom-set-variables
   '(custom-enabled-themes (quote (leuven))))
  (custom-set-faces
   '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 90 :width normal))))))


;; Backup settings.
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.s"))
 delete-old-versions t
 kept-new-versions 3
 kept-old-versions 2
 version-control t)


;; Preferences.
(set-language-environment "UTF-8")
(put 'narrow-to-region 'disabled nil)
(setq inhibit-splash-screen t)
(setq tab-width 4)
(setq org-src-fontify-natively t)
(setq-default indent-tabs-mode nil)
;; (setq scroll-margin 3)

;; Useful minor-modes setups!
(global-auto-revert-mode 1)
(global-linum-mode 1)
(show-paren-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(electric-pair-mode 1)
(add-hook 'minibuffer-setup-hook '(lambda () (interactive) (electric-pair-mode 0)))
(add-hook 'minibuffer-exit-hook '(lambda () (interactive) (electric-pair-mode 1)))



;; Auxiliary settings.
(setq find-function-C-source-directory "C:/emacs/src/")
; (setq default-directory "C:/Users/Shellay/OneDrive/")

;; (when (package-installed-p 'evil)
;;   (evil-mode))

(when (fboundp 'rainbow-delimiters-mode)
  (define-globalized-minor-mode global-rainbow-delimiters-mode
    rainbow-delimiters-mode rainbow-delimiters-mode)
  (global-rainbow-delimiters-mode 1))

(when (fboundp 'pretty-lambdada-mode)
  (define-globalized-minor-mode global-pretty-lambda-mode
    pretty-lambda-mode pretty-lambda-mode)
  (global-pretty-lambda-mode))

(setq-default c-basic-offset 4)
(setq-default outline-blank-line t)

(my-light-theme)


;; Fundamental c++

(add-hook 'c++-mode-hook
	  (lambda ()
	    (define-key c++-mode-map (kbd "C-c C-,")
	      (lambda ()
		(interactive)
		(let* ((in (buffer-file-name))
		       (out (substring in 0 -4)))
		  ;; (compile (format "g++ %s -g -o %s -std=c++11" in out))
		  ;; (compile (format "g++ %s -g -o %s -std=c++0x" in out))
		  (compile (format "g++ %s -g -o %s" in out)))))))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (define-key c++-mode-map (kbd "C-c C-.")
	      (lambda ()
		(interactive)
		(let* ((in (buffer-file-name))
		       (out (substring in 0 -4)))
		    (shell-command out))))))

(when (fboundp 'evil-mode)
  (add-hook
   'evil-mode-hook
   '(lambda ()
      (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
      (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
      (evil-set-initial-state 'shell-mode 'emacs)
      (evil-set-initial-state 'comint-mode 'emacs)
      (evil-set-initial-state 'compilation-mode 'emacs)
      (evil-set-initial-state 'gud-mode 'emacs)
      (evil-set-initial-state 'inferior-lisp 'emacs)
      (evil-set-initial-state 'help-mode 'emacs)
      (evil-set-initial-state 'inferior-python-mode 'emacs)
      (evil-set-initial-state 'dired-mode 'emacs)))
  (add-hook
   'evil-mode-hook
   '(lambda ()
      (define-key evil-normal-state-map (kbd "SPC `") (lambda (arg)
							(interactive "P")
							(insert-pair arg ?` ?`)))
      )))
