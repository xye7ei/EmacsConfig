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
(setq inhibit-splash-screen t)
(set-language-environment "UTF-8")
(put 'narrow-to-region 'disabled nil)


;; Useful minor-modes setups!
(global-auto-revert-mode 1)
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

; (when (package-installed-p 'evil)
;   (evil-mode))

; (when (package-installed-p 'rainbow-delimiters)
;   (define-globalized-minor-mode global-rainbow-delimiters-mode
;     rainbow-delimiters-mode rainbow-delimiters-mode)
;   (global-rainbow-delimiters-mode 1))

; (when (package-installed-p 'pretty-lambdada)
;   (define-globalized-minor-mode global-pretty-lambda-mode
;     pretty-lambda-mode pretty-lambda-mode)
;   (global-pretty-lambda-mode))

(setq-default c-basic-offset 4)
