;; Package settings
(require 'package)

(custom-set-variables
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))) 
 '(backup-directory-alist
   (quote
    (("tramp-file-name-regexp" . "~/.emacs.s")
     ("." . "~/.emacs.s"))))
 '(dired-listing-switches "-laX --group-directories-first")
 '(gud-pdb-command-name "python -m pdb")
 '(python-environment-directory "~/.virtualenvs")
 '(pyvenv-mode t)
 '(tramp-auto-save-directory "~/.emacs.s/tramp/")
 '(truncate-lines t)
 '(neo-window-fixed-size nil)
 '(neo-window-width 40)
 )

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
(setq tab-width 8)
(setq buffer-file-coding-system 'utf-8-unix)
(setq c-basic-offset 2)
(setq outline-blank-line t)
(setq-default indent-tabs-mode nil)

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
(when (boundp 'global-undo-tree-mode)
  (global-undo-tree-mode))
(global-hl-line-mode)
;; (global-linum-mode)
;; (winner-mode)
;; (ido-mode)


;; Compilation convenience
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c M-c") 'recompile)

(setq compilation-buffer-name-function
      (lambda (m-mode)
        (concat "==="
                (replace-regexp-in-string "===" ""
                                          (buffer-name))
                "===")))


;; Debugging convenience
(add-hook 'gud-mode-hook 'gud-tooltip-mode)

(global-set-key (kbd "<f7>") 'gud-step)
(global-set-key (kbd "<f8>") 'gud-next)
(global-set-key (kbd "S-<f8>") 'gud-finish)


;; Language independent hooks
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode 1)))


;; C/C++/Java stuff
(defun my-insert-c-block-comment (arg)
  (interactive "P")
  (save-excursion
    ;; Without marking, `insert-pair' is unavailable for empty target.
    (when (not mark-active)
      (set-mark (point)))
    (insert-pair arg "/*" "*/"))
  (forward-char 2))


;; Python stuff
(custom-set-variables
 '(gud-pdb-command-name "python -m pdb"))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c C-k")
              (lambda () (interactive)
                (compile (format "python \"%s\"" (buffer-file-name)))))
            (define-key python-mode-map (kbd "C-c C-d")
              (lambda () (interactive)
                (pdb (format "python -m pdb \"%s\"" (buffer-file-name)))))))
;; Problem by integration of Python module `pyreadline'
;; https://github.com/gregsexton/ob-ipython/issues/28
;; This issue raises possibly in Emacs > 25.0.
(setq python-shell-completion-native-enable nil)
;; Trick to use pprint within Python shell
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


;; Anaconda Python support. Use non-conflicting keys w.r.t. evil-mode.
(when (fboundp 'anaconda-mode)
  (require 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook
            (lambda ()
              (define-key anaconda-mode-map (kbd "C-c f d") 'anaconda-mode-find-definitions)
              (define-key anaconda-mode-map (kbd "C-c f a") 'anaconda-mode-find-assignments)
              (define-key anaconda-mode-map (kbd "C-c f r") 'anaconda-mode-find-references)
              (define-key anaconda-mode-map (kbd "C-c g d") 'anaconda-mode-find-definitions)
              (define-key anaconda-mode-map (kbd "C-c g a") 'anaconda-mode-find-assignments)
              (define-key anaconda-mode-map (kbd "C-c g r") 'anaconda-mode-find-references)
              (define-key anaconda-mode-map (kbd "C-c g b") 'anaconda-mode-go-back)
              (define-key anaconda-mode-map (kbd "C-c d") 'anaconda-mode-show-doc)
              (define-key anaconda-mode-map (kbd "C-c s d") 'anaconda-mode-show-doc))))


;; XML folding support

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-hook 'nxml-mode-hook 'hs-minor-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))


;; Aux minor mode settings
(when (fboundp 'rainbow-delimiters-mode)
  (define-globalized-minor-mode my-global-rainbow-delimiters-mode
    rainbow-delimiters-mode
    (lambda () (rainbow-delimiters-mode-enable)))
  (my-global-rainbow-delimiters-mode 1))

(when (fboundp 'evil-mode)
  (require 'evil)
  (setq evil-want-C-u-scroll t))


;; Further utilities
(add-hook 'hs-minor-mode-hook
          (lambda ()
            (define-key hs-minor-mode-map (kbd "M-i")
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


;; Funny tools

(defun split-window-multiple-ways (x y)
  "Split the current frame into a grid of X columns and Y rows."
  ;; https://www.emacswiki.org/emacs/GridLayout
  (interactive "nColumns: \nnRows: ")
  ;; one window
  (delete-other-windows)
  (dotimes (i (1- x))
      (split-window-horizontally)
      (dotimes (j (1- y))
	(split-window-vertically))
      (other-window y))
  (dotimes (j (1- y))
    (split-window-vertically))
  (balance-windows))

(defun dict (arg)
  (interactive "MQuery the word: ")
  (eww (format "https://en.wiktionary.org/wiki/%s" arg)))

(defun wiki (arg)
  (interactive "MQuery the concept: ")
  (eww (format "https://en.wikipedia.org/wiki/%s" arg)))


;; More
(setq frame-title-format "%b")

(eval-after-load 'smartparens
  '(progn
     (dolist (m '(compilation-mode
                  comint-mode
                  shell-mode))
       (sp-local-pair m "(" nil :unless nil)
       (sp-local-pair m "[" nil :unless nil)
       (sp-local-pair m "'" nil :unless nil)
       (sp-local-pair m "\"" nil :unless nil))))

(condition-case err
    (require 'dired-x)
  (error
   (message "Failed to require 'dired-x")))


(global-set-key (kbd "M-n") nil)
(global-set-key (kbd "M-n n") 'gud-next)
(global-set-key (kbd "M-n s") 'gud-step)
(global-set-key (kbd "M-n r") 'gud-cont)
(global-set-key (kbd "M-n b") 'gud-break)


(global-set-key (kbd "<f5>") (lambda () (interactive)
                               (revert-buffer t t t)))


(defun sql-command ()
  (interactive)
  (unwind-protect
      (save-excursion
        (when (not mark-active)
          (mark-paragraph))
        (setq stmt (buffer-substring (mark) (point)))
        (goto-line 0)
        (deactivate-mark)
        (setq sql-pfx (buffer-substring (+ 3 (line-beginning-position)) (line-end-position)))
        (setq sql-cmd (concat sql-pfx
                              " -wc \""
                              (replace-regexp-in-string "\"" "\\\\\"" stmt)
                              "\""))
        (setq sql-cmd (concat "echo \"$(" sql-cmd ")\""))
        (async-shell-command sql-cmd
                             (concat "===SQL: " (buffer-name) "==="))
        )))

(add-hook 'sql-mode-hook
          (lambda ()
            (define-key sql-mode-map (kbd "C-c <SPC>") 'sql-command)))

(add-to-list 'evil-emacs-state-modes 'term-mode)
(add-to-list 'evil-emacs-state-modes 'shell-mode)
(add-to-list 'evil-emacs-state-modes 'dired-mode)


(add-hook 'term-mode-hook (lambda () (interactive) (turn-off-evil-mode)))

(defalias 'rnb 'rename-buffer)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-iso-lefttab>")
                (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "<M-down>") 'windmove-down)
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-left>") 'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)

(global-set-key (kbd "<M-S-left>") 'previous-buffer)
(global-set-key (kbd "<M-S-right>") 'next-buffer)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'inferior-python-mode-hook 'ansi-color-for-comint-mode-on)


;; 
(message "My init.el buffer loading done.")
