(defun load-sml ()
  (interactive)
  (require 'sml-mode)
  (add-hook 'sml-mode-hook
	    '(lambda () (define-key sml-mode-map
		     (kbd "C-M-x")
		     'sml-send-function))))

(global-set-key (kbd "<apps> <apps> m") 'load-sml)
