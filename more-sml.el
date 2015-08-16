(defun load-sml ()
  (interactive)
  (when (not (package-installed-p 'sml-mode))
    (package-refresh-contents)
    (package-install 'sml-mode))
  (require 'sml-mode)
  (add-hook 'sml-mode-hook
	    '(lambda () (define-key sml-mode-map
		     (kbd "C-M-x")
		     'sml-send-function))))

(global-set-key (kbd "<apps> <apps> m") 'load-sml)
