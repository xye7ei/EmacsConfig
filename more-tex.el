;;; Tiny adaption for using TeX.
(add-hook 'org-mode-hook
	  '(lambda () (require 'org)
	     (setq org-format-latex-options
		   (plist-put org-format-latex-options :scale 1.4))))

(set-default 'preview-scale-function 1.2)

(defun my-expand-TeX-fold-env ()
  (dolist (c '(("[figure]" ("figure"))
	       ("[equation]" ("equation"))
	       ("[itemize...]" ("itemize"))
	       ("[tabular]" ("tabular"))
	       ("[align*]" ("align*"))))
    (add-to-list 'TeX-fold-env-spec-list c)))

(defun my-TeX-to-onerive ()
  (interactive)
  (copy-file (buffer-file-name)
	     (format "~/OneDrive/Active/%s" (buffer-name))))

(defun my-TeX-config () 
  (my-expand-TeX-fold-env)
  (define-key TeX-mode-map (kbd "C-c C-a") 'my-TeX-to-onerive))

;;; Hook my configs to fold-mode...
;;; then hook fold-mode to TeX-mode.
(add-hook 'TeX-fold-mode-hook 'my-TeX-config)
(add-hook 'latex-mode-hook 'TeX-fold-mode) 
(add-hook 'TeX-mode-hook 'TeX-fold-mode)
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
