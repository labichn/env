
;; A not too bad development environment for scala
;; Dependencies:
;; scamacs from https://github.com/RayRacine/scamacs.git
;;   cedet from http://git.randomsample.de/cedet.git
;;  ensime from https://github.com/aemoncannon/ensime.git
;;     ecb from 

(setq scamacs-root-dir-path "~/code/ext/scamacs/")
(setq       cedet-file-path "~/code/ext/cedet/cedet-devel-load.el")
(setq       ensime-dir-path "~/code/ext/ensime/dist_2.9.2/elisp/")
(setq          ecb-dir-path "~/code/ext/ecb/")
(setq   scala-mode-dir-path (concat scamacs-root-dir-path "scala/"))
(setq      scamacs-dir-path (concat scamacs-root-dir-path "scamacs/"))

(setq stack-trace-on-error t)

(add-to-list 'load-path scala-mode-dir-path)
(load-file cedet-file-path)
(add-to-list 'load-path ecb-dir-path)
(add-to-list 'load-path ensime-dir-path)
(add-to-list 'load-path scamacs-dir-path)

(require 'cedet)
(require 'semantic/analyze)
(provide 'semantic-load)
(require 'scala-mode-auto)
;(require 'ensime-ecb)
;(require 'ensime-layout-defs)
(require 'ensime)

;; * This enables the database and idle reparse engines for CEDET
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode,
;;   imenu support, and the semantic navigator for CEDET
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;;   for CEDET
(semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberant ctags if you have it installed.
;;   For CEDET.
(semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languages only via ctags.
;;(semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'ensime-ecb '(lambda () (ecb-show-sources-in-directories-buffer)))
