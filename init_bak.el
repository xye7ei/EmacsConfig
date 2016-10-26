;; This file incorporate some fundamental extensive functions.

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


;; Package activation.
;; - Make it called-by-need to save startup performance.

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


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
;; (setq scroll-margin 3)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default outline-blank-line t)


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

(my-light-theme)



;; Auxiliary settings.
;; (when (package-installed-p 'evil)
;;   (evil-mode))
(when (fboundp 'evil-mode)
  (require 'evil)
  (add-hook
   'evil-mode-hook
   (lambda ()
     (setq evil-want-C-u-scroll t)
     ;; (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
     ;; (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
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
   (lambda ()
     (define-key evil-normal-state-map (kbd "SPC `") (lambda (arg)
                                                       (interactive "P")
                                                       (insert-pair arg ?` ?`)))
     )))

(when (fboundp 'rainbow-delimiters-mode)
  (define-globalized-minor-mode global-rainbow-delimiters-mode
    rainbow-delimiters-mode rainbow-delimiters-mode)
  (global-rainbow-delimiters-mode 1))

(when (fboundp 'pretty-lambdada-mode)
  (define-globalized-minor-mode global-pretty-lambda-mode
    pretty-lambda-mode pretty-lambda-mode)
  (global-pretty-lambda-mode))


;; Fundamental C/C++

(global-set-key (kbd "<C-return>") 'compile)

(add-hook 'c-mode-hook
	  (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "gcc %s -Wall -g -o %s"
                         (buffer-file-name)
                         (file-name-sans-extension (buffer-file-name))))
            (define-key c-mode-map (kbd "C-c C-.")
              (lambda ()
                (interactive)
                (shell-command (file-name-sans-extension (buffer-file-name)))))))

(add-hook 'c++-mode-hook
	  (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "g++ %s -Wall -g -o %s"
                         (buffer-file-name)
                         (file-name-sans-extension (buffer-file-name))))
            (define-key c++-mode-map (kbd "C-c C-.")
              (lambda ()
                (interactive)
                (shell-command (file-name-sans-extension (buffer-file-name)))))))


(add-hook 'scala-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "scala %s" (buffer-file-name)))))

(add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "runhaskell %s" (buffer-file-name)))))


;; (pcase '(1 2)
;;   (`(,x ,y) (format "%s %s" x y)))

;; (print
;;  (macroexpand
;;   '(pcase '(1 2)
;;      (`(,x ,y) (format "%s %s" x y)))))

