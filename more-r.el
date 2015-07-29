;;; Load R support
(defun load-R ()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/ess-15.03-1/lisp/")
  (autoload 'R-mode "ess-site.el" "ESS" t)
  (add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
  (load "ess-site")
  (setq inferior-R-program-name "C:/Tools/R/R-3.2.1/bin/R.exe")
  (setq ess-eval-visibly-p nil)
  (setq ess-ask-for-ess-directory nil)
  (require 'ess-eldoc))

(global-set-key (kbd "<apps> <apps> r") 'load-R)
