(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (whiteboard)))
 ;; '(haskell-process-auto-import-loaded-modules t)
 ;; '(haskell-process-log t)
 ;; '(haskell-process-suggest-remove-import-lines t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 90 :width normal)))))

(defvar *my-config-dir* "~/Documents/GitHub/EmacsConfig/")
(defvar *code-dir* "~/OneDrive/Code/")

(load-file (concat *my-config-dir* "more.el"))
