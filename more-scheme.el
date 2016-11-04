(defun more-geiser ()
  (unwind-protect
      (progn

	(require 'geiser)

	(setq geiser-impl-installed-implementations '(racket))
	(setq geiser-active-implementations '(racket))

	;; (setq scheme-program-name "C:/Program Files/Racket/Racket.exe")
	;; (setq geiser-racket-binary "C:/Program Files/Racket/Racket.exe")

	;; (defun geiser-eval-till-here (start end)
	;;   (interactive "rP")
	;;   (geiser-eval-region 1 (point)))

	;; (defun geiser-eval-till-here () 
	;;   (geiser-eval-region 1 (point)))

	;; (defun geiser-more-keys ()
	;;   (define-keys
	;;     (list geiser-mode-map)
	;;     (list "C-c M-h" 'geiser-eval-till-here)))

	(add-hook 'scheme-mode-hook 'geiser-mode)

	(global-set-key (kbd "<apps> <apps> s") 'run-racket)

	;; 1. Petite Scheme
	;; (push "~/.emacs.d/iuscheme" load-path)
	;; (require 'iuscheme)
	;; (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
	;; (autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
	;; (push '("//.ss" . scheme-mode) auto-mode-alist)
	;; (custom-set-variables '(scheme-program-name "petite"))
	;; (setq scheme-program-name 
	;; 	"C:/Tools/ChezSchemeVersion84/bin/ti3nt/petite.exe")

	)
    (message "Geiser failed.")))

;; (when (not (package-installed-p 'geiser))
;;   (progn (package-refresh-contents)
;; 	 (package-install 'geiser)))

(more-geiser)
