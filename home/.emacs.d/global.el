;; I like me my lines and columns
(setq line-number-mode t)
(setq column-number-mode t)

;; tab shenanigans
(setq-default indent-tabs-mode nil)
(setq-default c-basic-indent 2)
(setq tab-width 2)

;; who needs a menu?
(menu-bar-mode -1)
(tool-bar-mode -1)
;; or a scrollbar?
(scroll-bar-mode -1)

;; insert lambda
(global-set-key (kbd "C-c l") (lambda () (interactive) (insert "λ")))
;; indent and untabify buffer
(global-set-key (kbd "C-c i") (lambda () (interactive) (indent-untabify-whole-buffer)))

;; aspell for spelling
(setq-default ispell-program-name "aspell")
