(setq ocaml-dir-path "/home/nick/.emacs.d/ocaml/")
(add-to-list 'load-path ocaml-dir-path)
(require 'tuareg)
;(setq auto-mode-alist
;      (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist 
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
