(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "sumatraPDF %s.pdf" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))))

;;; Tiny adaption for using TeX.
(add-hook 'org-mode-hook
	  '(lambda () (require 'org)
	     (setq org-format-latex-options
		   (plist-put org-format-latex-options :scale 1.4))))

(set-default 'preview-scale-function 1.2)

(defun my-expand-TeX-fold-env ()
  (dolist (c '(("[figure]" ("figure"))
	       ("[equation]" ("equation"))
	       ("[equation*]" ("equation*"))
	       ("[itemize...]" ("itemize"))
	       ("[tabular]" ("tabular"))
	       ("[align]" ("align"))
	       ("[align*]" ("align*"))
	       ("[center]" ("center"))))
    (add-to-list 'TeX-fold-env-spec-list c)))

(defun my-TeX-to-onerive ()
  (interactive)
  (copy-file (buffer-file-name)
	     (format "~/OneDrive/Active/%s" (buffer-name))
	     1))

(defun my-TeX-config () 
  (my-expand-TeX-fold-env)
  (define-key TeX-mode-map (kbd "C-c C-a") 'my-TeX-to-onerive))

;;; Hook my configs to fold-mode...
;;; then hook fold-mode to TeX-mode.
(add-hook 'TeX-fold-mode-hook 'my-TeX-config)
(add-hook 'latex-mode-hook 'TeX-fold-mode) 
(add-hook 'TeX-mode-hook 'TeX-fold-mode)
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
