(show-paren-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1) 
(electric-pair-mode 1)
(add-hook 'minibuffer-setup-hook '(lambda () (interactive) (electric-pair-mode 0)))
(add-hook 'minibuffer-exit-hook '(lambda () (interactive) (electric-pair-mode 1)))

(when (package-installed-p 'evil)
  (evil-mode))

(when (package-installed-p 'rainbow-delimiters)
  (define-globalized-minor-mode global-rainbow-delimiters-mode
    rainbow-delimiters-mode rainbow-delimiters-mode)
  (global-rainbow-delimiters-mode 1))

(when (package-installed-p 'pretty-lambdada)
  (define-globalized-minor-mode global-pretty-lambda-mode
    pretty-lambda-mode pretty-lambda-mode)
  (global-pretty-lambda-mode))
