;;;; open modules and modules above point

(defun tuareg-get-indent-lvl  ()
  "returns the indent lvl of phrase at point (as a string)"
  (let* ((start (nth 0 (tuareg-discover-phrase)))
         (startline (progn (beginning-of-line) (point)))
         (indent (buffer-substring startline start)))
    indent))


(defun tuareg-strip-end-double-semi (str)
  (let ((len (length str)))
    (if (and (>= len 2)
             (string= (subseq str (- len 2)) ";;"))
        (subseq str 0 (- len 2))
      str)))


(defun tuareg-previous-phrase ()
  "Skip to the beginning of the previous phrase.  It is redefined here
from tuareg.el because the original is broken.  This version appears
to be more robust"
  (interactive)
  (beginning-of-line)
  (tuareg-skip-to-end-of-phrase)
  (tuareg-find-phrase-beginning))


(defun tuareg-get-opened-modules-at-point-aux (acc)
  (let* ((curpoint (point))
         (newpoint (progn (tuareg-previous-phrase) (point))))
    (if (= curpoint newpoint) acc
      (let* ((start (point))
             (endoflinepoint (progn (end-of-line) (point)))
             (linestr (buffer-substring start endoflinepoint))
             (linestr (tuareg-strip-end-double-semi linestr))
             (tokenlist (split-string linestr)))
        (goto-char start)
        (if (string= (car tokenlist) "open")
            (tuareg-get-opened-modules-at-point-aux (cons (cadr tokenlist) acc))
          (tuareg-get-opened-modules-at-point-aux acc))))))


(defun tuareg-get-opened-modules-at-point ()
  "returns a list of modules open at point"
  (save-excursion (tuareg-get-opened-modules-at-point-aux nil)))


(defun tuareg-get-modules-with-indent (indent acc)
  "return a list of all modules with this indentation above point"
  (let ((modpos (re-search-backward (concat "^" 
                                            indent
                                            "module ")
                                    nil t)))
    (if (null modpos) acc
      (let* ((modline (buffer-substring modpos
                                        (progn (end-of-line) (point))))
             (modname (cadr (split-string modline))))

        (beginning-of-line)
        (tuareg-get-modules-with-indent indent (cons modname acc))))))




(defun tuareg-get-modules-above-point-aux (modpaths)
  "find all the modules (above point) and their full paths.  The
return value is a list of pairs: (module short name, module long name)"
  (let ((pos (point))
        (indentlvl (tuareg-get-indent-lvl)))
    (if (= (length indentlvl) 0)
        (let* ((finalline (buffer-substring pos
                                            (progn (end-of-line) (point))))
               (finalparts (split-string finalline)))
          (if (string= (car finalparts) "module")
              (cons (cons (cadr finalparts) (cadr finalparts)) modpaths)
            modpaths))
      (let* ((parentindent (subseq indentlvl 2))
             (parentpos (re-search-backward (concat "^" 
                                                    parentindent
                                                    "module ")))
             (parentline (buffer-substring parentpos
                                           (progn (end-of-line) (point))))
             (parentname (cadr (split-string parentline))))
        (ns-narrow-to-region pos parentpos)
        (goto-char (point-max))
        (let* ((mods (tuareg-get-modules-with-indent indentlvl nil))
               (modsproper (mapcar (lambda (x) (cons x x)) mods)))
          (ns-widen)
          (goto-char parentpos)
          (tuareg-get-modules-above-point-aux
           (mapcar (lambda (x) (cons (car x) (concat parentname "." (cdr x))))
                   modsproper)))))))


(defun tuareg-get-modules-above-point ()
  (save-excursion (tuareg-get-modules-above-point-aux nil)))
;;;; open modules and modules above point




;;;; make fake mod from modtype
(define-key tuareg-mode-map "\C-cf" 'tuareg-make-mod-from-modtypes)

(defun tuareg-parse-modtypename-on-pointline ()
  "parses the module type name in a \"module type BLAH = ...\" at point"
  (save-excursion
    (let ((modtypeline (buffer-substring 
                        (progn (beginning-of-line) (point))
                        (progn (end-of-line) (point)))))
      (destructuring-bind (junk good) (pgu-string-split modtypeline "module[ ]*type[ ]*")
        (destructuring-bind (good2 junk) (pgu-string-split good "[a-zA-Z0-9_]+")
          good2)))))

(defun tuareg-make-fake-args (num acc)
  (if (<= num 0) acc
    (tuareg-make-fake-args (- num 1) (concat acc " a" (number-to-string num)))))

(defun tuareg-conv-typesigvals-to-lets (str acc)
  (destructuring-bind (before val after) 
      (pgu-string-split2 (concat " " str) "[ \n]+\\(val\\|type\\)[ \n]+")
    ;; if no more
    (if (string= val "")
        (concat acc "\nend;;")

    ;; if it's a type
      (if (string-match "type" val)
          (destructuring-bind (sig junk junk2) 
              (pgu-string-split2 after "[ \n]+\\(val\\|type\\)[ \n]+")
            (tuareg-conv-typesigvals-to-lets (concat junk junk2) (concat acc val sig " = int\n")))

    ;; if it's a val
        (destructuring-bind (valname after) (pgu-string-split after "[a-zA-Z0-9_]+")
          (destructuring-bind (sig junk junk2) 
              (pgu-string-split2 after "[ \n]+\\(val\\|type\\)[ \n]+")
            (let* ((sig (if (string= sig "") junk2 sig))
                   (numargs (- (length (split-string sig "->" t)) 1)))
              (tuareg-conv-typesigvals-to-lets 
               after 
               (concat acc "let " valname (tuareg-make-fake-args numargs "") 
                       " = failwith \"fake\"\n")))))))))

(defun tuareg-make-modstr-from-modtypestr (str)
  (let ((acc "")
        (curr str))
    ; get rid of type in module type
    (destructuring-bind (mod junk rest) (pgu-string-split2 curr "[ \n]+type[ \n]+")
      (setq acc (concat acc mod))
      (setq curr rest))
    ; module / rest
    (destructuring-bind (modtypename rest) (pgu-string-split curr "[a-zA-Z0-9_]+")
      (setq acc (concat acc " " (capitalize modtypename) " : " modtypename))
      (setq curr rest))
    ; module State : STATE / rest
    (destructuring-bind (junk rest) (pgu-string-split curr "[ \n]+sig[ \n]+")
      (setq acc (concat acc " = struct\n"))
      (setq curr rest))
    (concat acc (tuareg-conv-typesigvals-to-lets curr ""))))


(defun tuareg-make-mod-from-modtype-at-point ()
  "makes a fake module for the module type at point"
  (interactive)
  (save-excursion
    (let* ((modtypename (tuareg-parse-modtypename-on-pointline))
           (NU (set-buffer tuareg-interactive-buffer-name))
           (modtypestr (tuareg-get-tell modtypename))
           ;(DBG (message "%s" modtypestr))
           (NU (goto-char (point-max)))
           (NU (comint-send-string 
                tuareg-interactive-buffer-name
                (tuareg-make-modstr-from-modtypestr modtypestr)))
           (NU (comint-send-input))
           (NU (display-buffer tuareg-interactive-buffer-name)))
      nil)))

(defun tuareg-make-mod-from-modtypes-aux ()
  (if (search-forward-regexp "module[ \n]+type" nil t)
      (progn
        (tuareg-make-mod-from-modtype-at-point)
        (end-of-line)
        (save-excursion (tuareg-wait-for-output))
        (sleep-for 0.1)
        (tuareg-make-mod-from-modtypes-aux))
    nil))


(defun tuareg-make-mod-from-modtypes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (tuareg-make-mod-from-modtypes-aux)))


(defun tuareg-makewrite-mod-from-modtype-at-point ()
  "makes a fake module for the module type at point and puts it
directly after the module type"
  (interactive)
  (save-excursion
    (let* ((modtypename (tuareg-parse-modtypename-on-pointline))
           (modtypestr (tuareg-get-tell (concat modtypename "!")))
           (modstr (tuareg-make-modstr-from-modtypestr modtypestr)))
      (search-forward-regexp "^end" nil t)
      (end-of-line)
      (insert (replace-regexp-in-string "\n" " " modstr)))))

;;;; make fake mod from mod type



;;;; improved eval-region
(define-key tuareg-mode-map "\C-c\C-r" 'tuareg-camltop-eval-region)

(defun tuareg-let-blockp-aux (lvl stuffp list)
  (cond
   ((null list) (not (and (= lvl 0) stuffp)))
   ((string= (car list) "let") (tuareg-let-blockp-aux (+ lvl 1) nil (cdr list)))
   ((string= (car list) "in") (tuareg-let-blockp-aux (- lvl 1) nil (cdr list)))
   (t (tuareg-let-blockp-aux lvl t (cdr list)))))


(defun tuareg-let-blockp (str)
  "is the input str a block of let-ins without any stuff to evaluate
after it.  If it is, typically we want to turn the let-ins into
let-doublesemis"
  (tuareg-let-blockp-aux 0 nil (split-string str "[ \f\t\n\r\v;]+" t)))


(defun tuareg-convert-let-block-aux (lvl list acclist)
  (cond
   ((null list) 
    (let* ((lasttok (car acclist))
           (lasttok (tuareg-strip-end-double-semi 
                     (car (reverse (split-string lasttok)))))
           (lasttok (concat lasttok ";;\n")))
      (apply 'concat (reverse (cons lasttok (cdr acclist))))))
      

   ((string= (car list) "let")
    (tuareg-convert-let-block-aux (+ lvl 1) (cdr list)
                                  (cons (concat (car list) " ") acclist)))

   ((string= (car list) "in")
    (let ((nexttok (if (= lvl 1) ";;\n" (concat (car list) "\n"))))
      (tuareg-convert-let-block-aux (- lvl 1) (cdr list) (cons nexttok acclist))))

   (t (tuareg-convert-let-block-aux lvl (cdr list)
                                    (cons (concat (car list) " ") acclist)))))


(defun tuareg-convert-let-block (str)
  (tuareg-convert-let-block-aux 0 (split-string str) nil))



(defun tuareg-camltop-eval-region ()
  (interactive)
  (let* ((region (buffer-substring (region-beginning) (region-end)))
         (region (if (tuareg-let-blockp region)
                    (tuareg-convert-let-block region)
                   region))
         (modpaths (tuareg-get-modules-above-point))
         (openmods (tuareg-get-opened-modules-at-point))
         (opens (apply `concat (mapcar
                                (lambda (mod) 
                                  (let* ((fullpath (cdr (assoc mod modpaths)))
                                         (fullpath (if (null fullpath) mod fullpath)))
                                    (concat "open " fullpath ";;\n")))
                                openmods)))
         (mods (apply `concat (mapcar
                               (lambda (mod)
                                 (concat "open " (cdr mod) ";;\n"))
                               modpaths)))
         (text (concat "module TEMP_REGION = struct\n" 
                       opens
                       mods
                       region
                       "\nend;; include TEMP_REGION;;\n")))
    (tuareg-camltop-compile-str text nil)))
;;;; improved eval-region





;;;; camltop debugging
(define-key tuareg-mode-map "\C-cv" 'tuareg-camltop-showcurrentscope)
(define-key tuareg-mode-map "\C-cb" 'tuareg-camltop-push-baseframe)
(define-key tuareg-mode-map "\C-cp" 'tuareg-camltop-push-frame)
(define-key tuareg-mode-map "\C-co" 'tuareg-camltop-pop-frame)
(define-key tuareg-mode-map "\C-cs" 'tuareg-camltop-step-in-function)
(define-key tuareg-mode-map "\C-ca" 'tuareg-camltop-assign-args)


;; each element of this stack (list) will be a tuple (name, args,
;; buffername, pos)
(defvar *tuareg-camltop-stack-frame* nil)

(defun tuareg-camltop-send-cmd (cmd)
  (save-excursion
    (set-buffer (get-buffer tuareg-interactive-buffer-name))
    (goto-char (point-max))
    (comint-send-string tuareg-interactive-buffer-name cmd)
    (comint-send-input)
    (display-buffer tuareg-interactive-buffer-name)))

;; function step in
;; 1) highlight the function call -> do \C-c s to:
;;   a) parse the function name and args
;;   b) push the (function name, args, buffername, pos) onto debug stack
;;   c) #push the function name in the toplevel
;;
;; 2) highlight the function being called -> \C-c a to:
;;   a) assign the arguments their values in the toplevel
;;
(defun tuareg-camltop-parse-functioncall-aux (str plvl qlvl nestedarg parsedfuncall)
  (if (string= str "") parsedfuncall
    (destructuring-bind (pre mid post) (pgu-string-split2 str "[()\"]")
      (let ((pre (pgu-string-strip-trailing-spaces pre))
            (post (pgu-string-strip-trailing-spaces post)))
        (cond
         ((string= mid "") (if (string= post "") parsedfuncall
                             (cons (cons t post) parsedfuncall)))
                         
         ((string= mid "\"")
          (if (= qlvl 0)
              (tuareg-camltop-parse-functioncall-aux
               post
               plvl
               (+ qlvl 1)
               (if (> plvl 0) (concat nestedarg pre mid) mid)
               (if (> plvl 0) parsedfuncall
                 (if (string= pre "") parsedfuncall 
                   (cons (cons t pre) parsedfuncall))))
            (tuareg-camltop-parse-functioncall-aux
             post
             plvl
             (- qlvl 1)
             (if (> plvl 0) (concat nestedarg pre mid) "")
             (if (> plvl 0) parsedfuncall (cons (cons nil (concat nestedarg pre mid))
                                                parsedfuncall)))))

         ((string= mid "(")
          (if (= plvl 0)
              (tuareg-camltop-parse-functioncall-aux 
               post (+ plvl 1) qlvl mid (if (string= pre "") parsedfuncall
                                          (cons (cons t pre) parsedfuncall)))
            (tuareg-camltop-parse-functioncall-aux
             post (+ plvl 1) qlvl (concat nestedarg pre mid) parsedfuncall)))
              
         ((string= mid ")")
          (if (= plvl 1)
              (tuareg-camltop-parse-functioncall-aux
               post (- plvl 1) qlvl "" (cons (cons nil (concat nestedarg pre mid))
                                             parsedfuncall))
            (tuareg-camltop-parse-functioncall-aux
             post (- plvl 1) qlvl (concat nestedarg pre mid) parsedfuncall))))))))


(defun tuareg-camltop-parse-functioncall ()
  "returns a list of the function name and its arguments in a list,
one argument/functionname per item in the list"
  (let ((chunks (tuareg-camltop-parse-functioncall-aux
                 (pgu-string-replace
                  (buffer-substring (region-beginning) (region-end))
                  "[\n]" " " nil)
                 0 0 "" nil)))
    (pgu-list-flatten
     (mapcar (lambda (x) (if (car x) (split-string (cdr x))
                           (list (cdr x))))
             (reverse chunks)))))


(defun tuareg-camltop-step-in-function ()
  (interactive)
  (let* ((fparts (tuareg-camltop-parse-functioncall))
         (fname (car fparts))
         (fargs (cdr fparts))
         (bufname (buffer-name (current-buffer)))
         (pos (point)))
    (setq *tuareg-camltop-stack-frame*
          (cons (list fname fargs bufname pos) *tuareg-camltop-stack-frame*))
    (tuareg-camltop-send-cmd (concat "#push \"" fname "\";;"))))


(defun tuareg-camltop-assign-args ()
  (interactive)
  (let* ((args (tuareg-camltop-parse-functioncall))
         (vals (cadr (car *tuareg-camltop-stack-frame*))))
    (tuareg-camltop-send-cmd
     (concat (pgu-string-of-list "let " " = " "," args)
             (pgu-string-of-list "" ";;" "," vals)))))



(defun tuareg-camltop-showcurrentscope ()
  (interactive)
  (tuareg-camltop-send-cmd "#show \"scopedvalue\";;"))


(defun tuareg-camltop-push-baseframe ()
  "pushes the base frame"
  (interactive)
  (let* ((buf (current-buffer))
         (bufname (buffer-name buf))
         (path (buffer-file-name buf))
         (filename (file-name-nondirectory path))
         (filename (file-name-sans-extension filename))
         (modname (capitalize filename))
         (pos (point)))

    (setq *tuareg-camltop-stack-frame* 
          (cons (list modname nil bufname pos) *tuareg-camltop-stack-frame*))
    (tuareg-camltop-send-cmd (concat "#push \"" modname "\";;"))))


(defun tuareg-camltop-pop-frame ()
  "pops a stack frame"
  (interactive)
  (destructuring-bind (name args bufname pos) (car *tuareg-camltop-stack-frame*)
    (tuareg-camltop-send-cmd "#pop;;")
    (switch-to-buffer (get-buffer bufname))
    (goto-char pos)
    (setq *tuareg-camltop-stack-frame* (cdr *tuareg-camltop-stack-frame*))))

    


;; break point set
;; 1) set the break point with \C-c b
;;   a) make _temp variable equal to ref None in toplevel
;;   b) ask user for variables whose scope we want

;;;;; camltop debugging