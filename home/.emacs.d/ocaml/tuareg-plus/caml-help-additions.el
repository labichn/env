;; this improves caml-help by taking into account opened modules and module
;; aliases

(require 'gen-utils)
(require 'caml-help)
(require 'tuareg)
(require 'camltop-env)
(require 'tuareg-utils)

(define-key tuareg-interactive-mode-map "\C-ch" 'caml-help)


(defun caml-info-lookup-scan-aliases ()
  "Looks for modules aliases directives in caml files"
  (let (aliases id1 id2 mlist)
    (save-excursion
      (goto-char (point-min))
      (let ((module-regexp "^[ \t]*module[ \t]+\\([A-Z_][a-zA-Z0-9_]*\\)[ \t]+=[ \t]+\\([A-Z_][a-zA-Z0-9_]*\\(\\.[A-Z_][a-zA-Z0-9_]*\\)*\\)"))
	(while (re-search-forward module-regexp nil t)
	  (unless (tuareg-in-comment-p)
	    (setq id1 (match-string-no-properties 1))
	    (setq id2 (match-string-no-properties 2))
	    (when (and id2 (not (string= id2 "struct")))
	      (setq aliases 
		    (cons (cons id1 (split-string id2 "\\.")) aliases)))))))
    aliases))


;;; redefining caml-help from caml-help.el.  Now it will take into
;;; account module aliases and such.  Yes, I know taking advantage of
;;; dynamic scoping is bad form... and yet.. it's so easy!
(defun caml-help (arg)
  "Find documentation for OCaml qualified identifiers. 

It attemps to recognize an qualified identifier of the form
``Module . entry'' around point using function `ocaml-qualified-identifier'.

If Module is undetermined it is temptatively guessed from the identifier name
and according to visible modules. If this is still unsucessful,  the user is 
then prompted for a Module name. 

The documentation for Module is first seach in the info manual if available,
then in the ``module.mli'' source file. The entry is then searched in the documentation. 

Visible modules are computed only once, at the first call. 
Modules can be made visible explicitly with `ocaml-open-module' and
hidden with `ocaml-close-module'. 

Prefix arg 0 forces recompilation of visible modules (and their content)
from the file content. 

Prefix arg 4 prompts for Module and identifier instead of guessing values
from the possition of point in the current buffer. 
"
  (interactive "p")
  (mapcar 'ocaml-open-module (tuareg-environment-opens-above-point))
  (let ((module) (entry) (module-entry))
    (cond
     ((= arg 4)
      (or (and
           (setq module
		 (completing-read "Module: " (ocaml-module-alist)
				  nil t "" (cons 'hist 0)))
           (not (string-equal module "")))
          (error "Quit"))
      (let ((symbols
             (mapcar 'list
                     (ocaml-module-symbols
                      (assoc module (ocaml-module-alist))))))
        (setq entry (completing-read "Value: " symbols nil t)))
      (if (string-equal entry "") (setq entry nil))
      )
     (t
      (if (= arg 0) (setq ocaml-visible-modules 'lazy))
      (setq module-entry (ocaml-qualified-identifier))
      (setq entry (ocaml-buffer-substring (cdr module-entry)))
      (setq module
            (or (ocaml-buffer-substring (car module-entry))
                (let ((modules
                       (or (ocaml-find-module entry (ocaml-visible-modules))
                           (ocaml-find-module entry)))
                      (hist) (default))
                  (cond
                   ((null modules)
                    (error "No module found for entry %s" entry))
                   ((equal (length modules) 1)
                    (caar modules))
                   (t
                    (setq hist (mapcar 'car modules))
                    (setq default (car hist))
                    (setq module
                          (completing-read
                           (concat "Module: "
                                   (and default (concat "[" default "] ")))
                           modules nil t "" (cons 'hist 0)))
                    (if (string-equal module "") default module))
                   ))))
      ))

    (let ((module (or (cadr (assoc module (caml-info-lookup-scan-aliases))) module)))
      (message "Help for %s%s%s" module (if entry "." "") (or entry ""))
      (ocaml-goto-help module entry))))



;;; redefining ocaml-ocamldoc-info-add-entries from caml-help.el.  Now
;;; the find command it uses will follow symbolic links.  Yay.
;;; Another example of shamelessly taking advantage of dynamic
;;; scoping...
(defun ocaml-ocamldoc-info-add-entries (entries dir name)
  (let*
      ((module-regexp "^Node: \\([A-Z][A-Za-z_0-9]*\\)[^ ]")
       (command
        (concat
         "find -L " dir " -type f -regex '.*/" name
         "\\(.info\\|\\)\\([.]gz\\|\\)' -print0"
         " | xargs -0 zcat -f | grep '" module-regexp "'")))
    (message "Scanning info files in %s" dir)
    (save-window-excursion
      (set-buffer (get-buffer-create "*caml-help*"))
      (or (shell-command command (current-buffer)) (error "HERE"))
      (goto-char (point-min))
      (while (re-search-forward module-regexp (point-max) t)
        (if (equal (char-after (match-end 1)) 127)
            (let* ((module (match-string 1)))
              (if (assoc module entries) nil
                (setq entries
                      (cons (cons module (concat "(" name ")" module))
                            entries))
                ))))
      ; (kill-buffer (current-buffer))
      )
    entries))

(provide 'caml-help-additions)
