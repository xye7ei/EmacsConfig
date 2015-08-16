;;; Load R support
;;; Suppose program R is successfully installed and appended to
;;; environmental variable PATH (if under Windows);
;;; The package ESS is assumed not well merged into 'melpa
;;; repository. It's advised that ESS source are putted into
;;; file path ~/.emacs.d/ and loaded seperately from package
;;; management system.
(defun load-R ()
  (interactive)
  (when (not (package-installed-p 'ess))
    (package-refresh-contents)
    (package-install 'ess))
  ;; (add-to-list 'load-path "~/.emacs.d/ess-15.03-1/lisp/")
  (autoload 'R-mode "ess-site.el" "ESS" t)
  (add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
  (load "ess-site")
  (setq inferior-R-program-name "R.exe")
  (setq ess-eval-visibly-p nil)
  (setq ess-ask-for-ess-directory nil)
  (require 'ess-eldoc))

(global-set-key (kbd "<apps> <apps> r") 'load-R)
