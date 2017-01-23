;; Package settings
(require 'package)

(custom-set-variables
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")))))

(package-initialize)

(when (eq system-type 'windows-nt)
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
(set-language-environment "UTF-8")
(put 'narrow-to-region 'disabled nil)
(setq inhibit-splash-screen t)
(setq tab-width 4)
(setq buffer-file-coding-system 'utf-8-unix)
(setq indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq outline-blank-line t)
;; Preferences for org-mode
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Minor-modes
;; - close for simplifying views
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; - open for easy use
(electric-pair-mode)
(show-paren-mode)
(column-number-mode)
(global-auto-revert-mode)
(global-undo-tree-mode)
;; (global-linum-mode)
;; (winner-mode)
;; (ido-mode)


;; Compile commands
(global-set-key (kbd "<C-return>") 'compile)
(global-set-key (kbd "<C-M-return>") 'recompile)

(defun my-hook-compile-command (hook form)
  (eval `(add-hook ',hook (lambda ()
                            (set (make-local-variable 'compile-command)
                                 ,form)))))

(let ((bfn '(buffer-file-name))
      (bfn0 '(file-name-sans-extension (buffer-file-name))))
  (my-hook-compile-command 'c-mode-hook
			    `(format "gcc -std=c99 -g -Wall -O0 -o \"%s\" \"%s\""
				     ,bfn0 ,bfn))
  (my-hook-compile-command 'c++-mode-hook
			    `(format "g++ -std=c++14 -g -Wall -O0 -o \"%s\" \"%s\""
				     ,bfn0 ,bfn))
  (my-hook-compile-command 'python-mode-hook
			    (let ((py (if (eq system-type 'windows-nt) "python" "python3")))
			      `(format "%s \"%s\"" ,py ,bfn)))
  (my-hook-compile-command 'scala-mode-hook
			    `(format "scala \"%s\"" ,bfn))
  (my-hook-compile-command 'haskell-mode-hook
			    `(format "stack ghc \"%s\" -- -fno-code" ,bfn)))


;; C/C++ stuff
(defun my-insert-c-block-comment (arg)
  (interactive "P")
  (save-excursion
    ;; Without marking, `insert-pair' is unavailable for empty target.
    (when (not mark-active)
      (set-mark (point)))
    (insert-pair arg "/*" "*/"))
  (forward-char 2))

(add-hook 'scala-mode-hook
          (lambda ()
            (define-key scala-mode-map (kbd "C-M-;") 'my-insert-c-block-comment)))
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map (kbd "C-M-;") 'my-insert-c-block-comment)))


;; Haskell stuff
(defun my-insert-haskell-block-comment (arg)
  (interactive "P")
  (save-excursion
    ;; Without marking, `insert-pair' is unavailable for empty target.
    (when (not mark-active)
      (set-mark (point)))
    (insert-pair arg "{-" "-}"))
  (forward-char 2))


;; Python stuff
(custom-set-variables
 '(gud-pdb-command-name "python -m pdb"))
(setq my-python-command
      (if (eq system-type 'windows-nt) "python" "python3"))
(add-hook 'python-mode-hook
          (lambda ()
            (when (fboundp 'jedi:setup)
              (jedi:setup))
            (define-key python-mode-map (kbd "C-c C-k")
              (lambda () (interactive)
                (compile (format "%s \"%s\""
                                 my-python-command (buffer-file-name)))))
            ;; (define-key python-mode-map (kbd "C-c C-d") 'pdb)
            (define-key python-mode-map (kbd "C-c C-d")
              (lambda () (interactive)
                (pdb (format "%s -m pdb \"%s\"" my-python-command (buffer-file-name)))))))
;; Problem by integration of Python module `pyreadline'
;; https://github.com/gregsexton/ob-ipython/issues/28
;; This issue raises possibly in Emacs > 25.0.
(setq python-shell-completion-native-enable nil)

(setq python-shell-setup-codes `("
from pprint import pprint as __pprint
import sys as __sys

__ORIG_DISP_HOOK = __sys.displayhook

def __pp_hook(value):
    if None is not value:
        __builtins__._ = value
        __pprint(value)

__sys.displayhook = __pp_hook")
      )

(defun ipython ()
  (interactive)
  (if (equal python-shell-interpreter "python")
      (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter "python")))


;; Aux minor mode settings
(when (fboundp 'rainbow-delimiters-mode)
  (define-globalized-minor-mode my-global-rainbow-delimiters-mode
    rainbow-delimiters-mode
    (lambda () (rainbow-delimiters-mode-enable)))
  (my-global-rainbow-delimiters-mode 1))

(when (fboundp 'evil-mode)
  (setq evil-want-C-u-scroll t))


;; Further utilities
(add-hook 'hs-minor-mode-hook
          (lambda ()
            (define-key hs-minor-mode-map (kbd "C-`")
              (lambda (arg)
                "Easy folding with specified level."
                (interactive "c")
                (let ((val (- arg 48)))
                  (if (zerop val) (hs-show-all) (hs-hide-level val)))))))

(defun my-swap-pair ()
  (interactive)
  (save-excursion
    (let* ((point-open (point))
	   (point-close (scan-sexps point-open 1))
	   (open-old (char-after))
	   (open (car (alist-get open-old '((?\( ?\[)
					    (?\[ ?\{)
					    (?\{ ?\()
					    (?\' ?\")
					    (?\" ?\'))))))
      (when (not (eq open nil))
	(let ((close (car (alist-get open insert-pair-alist))))
	  (when (not (eq close nil))
	    (delete-char 1) (insert-char open) (goto-char point-close)
	    (delete-char -1) (insert-char close) (goto-char point-open)))))))

(global-set-key (kbd "M-[") 'my-swap-pair)
(global-set-key (kbd "C-M-]") 'delete-pair)
(global-set-key (kbd "C-M-=") 'hs-minor-mode)

;; Convenient editing
(global-set-key (kbd "C->") '(lambda () (interactive) (insert-string " -> ")))
(global-set-key (kbd "C-<") '(lambda () (interactive) (insert-string " <- ")))


;; Interface utitlies

(defun my-set-transparency (arg)
  (interactive "nInput alpha value: ")
  (set-frame-parameter nil 'alpha (list arg arg)))

(defun my-increase-face-size (x)
  (let ((h (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ h x))))

(global-set-key (kbd "C-S-d") (lambda () (interactive) (delete-backward-char 1)))
(global-set-key (kbd "C-=") (lambda () (interactive) (my-increase-face-size 5)))
(global-set-key (kbd "C--") (lambda () (interactive) (my-increase-face-size -5)))
(global-set-key (kbd "C-<up>") (lambda () (interactive) (scroll-down 2)))
(global-set-key (kbd "C-<down>") (lambda () (interactive) (scroll-up 2)))
