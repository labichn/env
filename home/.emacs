(defun safe-load (path)
  ;; Safely load a file or directory
  (if (file-exists-p path)
      (load-file path)
    (if (file-accessible-directory-p path)
	(add-to-list 'load-path path)
        (message "Could not load \"%s\"." path))))
(safe-load "~/.emacs.d/global.el")
(safe-load "~/.emacs.d/functions.el")
(safe-load "~/.emacs.d/scala-dev.el")
(safe-load "~/.emacs.d/racket-dev.el")
(safe-load "~/.emacs.d/ocaml-dev.el")
(safe-load "~/.emacs.d/haskell-dev.el")
(safe-load "~/.emacs.d/json.el")
(safe-load "~/.emacs.d/json-pretty-print.el")
(safe-load "~/.emacs.d/paredit.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
