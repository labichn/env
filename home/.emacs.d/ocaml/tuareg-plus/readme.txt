Some extra functionality for tuareg-mode.  Should not be used in conjunction
with ocaml-mode.

To turn it on add the following to your .emacs.  Note that your load-path will
probably be different.

  (setq load-path (cons "~/emacs/tuareg-plus" load-path))
  (add-hook 'tuareg-mode-hook
            '(lambda ()
               (require 'tuareg-plus)
               (require 'camltest)
               (setq compile-command "make")
               (setq ocaml-info-prefix "ocaml-indx")
               (tuareg-outline-create-outline)))

  (add-hook 'tuareg-interactive-mode-hook
            '(lambda ()
               (require 'tuareg-plus)))


Some tuareg settings I find useful.

  (custom-set-variables
    '(tuareg-in-indent 0)
    '(tuareg-lazy-paren nil)
    '(tuareg-let-always-indent nil)
    '(tuareg-let-indent 2)
    '(tuareg-library-path "~/app-data/godi/lib/ocaml/site-lib/")
    '(tuareg-manual-url "file:///home/peng/docs/ocaml/htmlman/index.html")
    '(tuareg-match-indent 0)
    '(tuareg-|-extra-unindent 2))
