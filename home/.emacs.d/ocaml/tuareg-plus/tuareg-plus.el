;; Additional features to tuareg mode.  These include:
;;
;; insta-help: (caml-help-additions.el) Brings up the manual info file for
;;             identifier at point.  This feature is based on that in
;;             ocaml-mode.  It is improved by its ability to auto-scan and
;;             take into account open modules and module alias.
;;
;; improved-eval: (camltop-compile.el) Improves the evaluation region command
;;                of tuareg-mode.  Instead of evaluating a highlighted region
;;                in the toplevel via comint, it writes the region to a
;;                temporary file and instructs the ocaml toplevel to "#use"
;;                the temporary file.  This is much faster for large regions.
;;                It also interprets toplevel directives hidden in comments,
;;                eg. "(*#load "foo.cmo";;*)".  This means you can include
;;                toplevel directives in your source files without the need to
;;                remove them for compilation.
;;
;; setup-env: (camltop-env.el) Sends a series of open statements to the
;;            toplevel so that it is in the name space of the expression at
;;            point.  In the past, evaluating (in the toplevel) an arbitrary
;;            expression in the middle of a source file may not work.  This is
;;            because it may depend on modules defined or open statements
;;            above the expression.  This provides functions to setup the
;;            toplevel to the exact environmental conditions at the expression
;;            so that it may be evaluated.  Note that it assumes the file is
;;            compiled and loaded.  Loading can be automated as well by a
;;            simple "(*#load "filename-or-library.cmo";;*)" hidden directive
;;            at the top of the file.
;;
;; completion: (tuareg-completion.el) Provides completion of identifier at
;;             point.  Completion is based on the identifiers available in the
;;             toplevel.  This means if you have defined a function in the
;;             toplevel, it is available for completion.  Completion takes
;;             into account open modules, module alias, etc.. as long as it
;;             has been entered in the toplevel.
;;
;; what-is: (tuareg-completion.el) Provides what-is support.  Pull up the
;;          value and type of identifier at point.  It is based on the
;;          toplevel so as long as it is defined in the toplevel, it is
;;          available.
;;
;; outline: Provides an outline view of the source file.  The outline view is
;;          in another buffer called *tuareg-outline*.  The view is in outline
;;          mode which allows you to expand, shrink nodes.

(require 'highlight)
(require 'gen-utils)
(require 'narrow-stack)
(require 'info)
(info-initialize)
(require 'tuareg-utils)
(require 'caml-help)
(require 'caml-help-additions)
(require 'camltop-compile)
(require 'camltop-env)
(require 'tuareg-completion)
(require 'tuareg-outline)


;;;;;;  Some misc general improvements to tuareg mode

;; redefining restart-process so it kills the actual buffer and creates a new
;; one.  This way is faster
(define-key tuareg-mode-map "\C-cr" 'tuareg-restart-process)
(define-key tuareg-interactive-mode-map "\C-cr" 'tuareg-restart-process)
(defun tuareg-restart-process ()
  (interactive)
  (save-excursion
    (set-buffer tuareg-interactive-buffer-name)
    (comint-kill-subjob)
    (delete-region (point-min) (point-max)))
  (tuareg-run-process-if-needed tuareg-interactive-program)
  (when tuareg-display-buffer-on-eval
    (display-buffer tuareg-interactive-buffer-name)))


;; custom compile that is framhist aware
(defun tuareg-compile (command &optional comint)
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
	  (read-from-minibuffer "Compile command: "
				command nil nil
				(if (equal (car compile-history) command)
				    '(compile-history . 1)
				  'compile-history))
	command))
    (consp current-prefix-arg)))
  (framehist:wpush)
  (compile command))
(define-key tuareg-mode-map "\C-c\C-c" 'tuareg-compile)

;; additional navigation
(define-key tuareg-mode-map "\C-c\C-p" 'tuareg-prev-block)
(define-key tuareg-mode-map "\C-c\C-n" 'tuareg-next-block)
(define-key tuareg-mode-map "\C-cn" 'tuareg-narrow-to-block)
(define-key tuareg-mode-map "\C-cc" 'tuareg-show-block-context)
(defun tuareg-next-block (&optional popout)
  "move to the next line same or lower indentation level.
Argument means a lower level (popping out)."
  (interactive "P")
  (let* ((curlvl (save-excursion
                   (goto-char (car (tuareg-discover-phrase)))
                   (tuareg-utils-indent-level)))
         (trglvl (if popout (if (>= curlvl 2) (- curlvl 2) 0) curlvl))
         (regexp (concat "^"
                         (pgu-foldNtimes trglvl (lambda (x) (concat x "[ ]")) 0 "")
                         "[a-z]")))
    (end-of-line)
    (re-search-forward regexp)
    (forward-char -1)))


(defun tuareg-prev-block (&optional popout)
  "move to the previous line of the same or lower indentation
level (popping out).  Argument means popping out"
  (interactive "P")
  (let* ((curlvl (save-excursion
                   (goto-char (car (tuareg-discover-phrase)))
                   (tuareg-utils-indent-level)))
         (trglvl (if popout (if (>= curlvl 2) (- curlvl 2) 0) curlvl))
         (regexp (concat "^"
                         (pgu-foldNtimes trglvl (lambda (x) (concat x "[ ]")) 0 "")
                         "[a-z]")))
    (forward-line 0)
    (re-search-backward regexp)
    (forward-char trglvl)))

(defun tuareg-narrow-to-block ()
  "narrow to the current enclosing block"
  (interactive)
  (save-excursion
    (let* ((beg (progn (tuareg-prev-block t) (forward-line 0) (point)))
           (end (progn (tuareg-next-block) (end-of-line) (point))))
      (ns-narrow-to-region beg end))))

;; hide of debug code
(defun tuareg-hide-dbg ()
  (interactive)
  
  (hlt-highlight-regexp-region
   (point-min)
   (point-max)
   "^[ ]*[(][*] DBG-BEG [*][)]\\(?:.*\n\\)*?[ ]*[(][*] DBG-END [*][)].*\n"
   nil
   nil
   nil
   nil)
  (hlt-hide-default-face (point-min) (point-max) hlt-last-face))

;; show debug code
(defun tuareg-show-dbg ()
  (interactive)
  (hlt-show-default-face hlt-last-face)
  (scroll-up 0))


(define-key tuareg-mode-map "\C-cl" 'tuareg-hide-dbg)
(define-key tuareg-mode-map "\C-cs" 'tuareg-show-dbg)


;; show the current block context
(defun tuareg-show-block-context (lvl)
  "show the current block context"
  (interactive "p")
  (let* ((res (save-excursion
                (pgu-doNtimes (lambda () (tuareg-prev-block t)) (if (<= lvl 0) 1 lvl))
                (let ((beg (progn (forward-line 0) (point)))
                      (end (progn (end-of-line) (point))))
                  (buffer-substring beg end)))))
    (message "%s" res)))


(provide 'tuareg-plus)
