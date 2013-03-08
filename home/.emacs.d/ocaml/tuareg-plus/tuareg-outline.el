(require 'tuareg)
(require 'tuareg-utils)
(eval-when-compile (require 'outline))
(define-key tuareg-mode-map "\C-c\C-u" 'tuareg-outline-create-outline-and-show)
(define-key tuareg-mode-map "\C-cu" 'tuareg-outline-create-outline)
(add-hook 'after-save-hook 'tuareg-outline-create-outline nil t)

;;;;; some variables
(defvar tuareg-outline-base-buffer nil "holds the buffer that this outline is for")
(defvar tuareg-outline-base-outline nil "holds the backing datastructure of this outline")

;;;;; function for getting a list of interesting locations and the type of
;;;;; those locations.
;;;;;
;;;;; type loc = int (char position)
;;;;; type loctype = 
;;;;;   | 0 (module)
;;;;;   | 1 (let expression)
;;;;; type locEntry = (loc,loctype)
;;;;; type locs = locEntry list
(defun tuareg-outline-heading-locs ()
  "returns a list of locEntry.  These are where the headings are.
Headings are either module or let statements "
  (let* ((module-locs (mapcar (lambda (loc) (cons loc 0)) (tuareg-utils-module-locs)))
         (let-locs (mapcar (lambda (loc) (cons loc 1)) (tuareg-utils-let-locs)))
         (all-locs (append module-locs let-locs)))
    (sort all-locs (lambda (a b) (< (car a) (car b))))))


;;;;; functions for creating the outline (tree)
;;;;;
;;;;; type indentlvl = int
;;;;; type outlineEntry = (indentlvl, string, type, loc)
;;;;; type node = (Let, entry) | (Module, entry, tree)
;;;;; type tree = node list
(defun tuareg-outline-make-outline-aux (locs acc maxindent)
  "goes through all the locations in 'locs' and makes an
  outlineEntry for all the good ones.  These are put on the
  accumulator 'acc'.  It is a list of all the good
  outlineEntries.  'maxindent' is the maximum indentation
  accepted for let statements.  If there is a let statements
  whose indent is greater than maxindent, it is skipped."
  (if (null locs) acc
    (let* ((charpos (car (car locs)))
           (type (cdr (car locs)))
           (indentlvl (tuareg-utils-indent-level charpos)))
      (cond
       ( (= type 0) ; module
         (tuareg-outline-make-outline-aux
            (cdr locs)
            (cons (list indentlvl (tuareg-utils-word-at-point charpos) 0 charpos) acc)
            (+ indentlvl 2)))
       ( (= type 1) ; let
         (if (> indentlvl maxindent)
             (tuareg-outline-make-outline-aux (cdr locs) acc maxindent)
           (tuareg-outline-make-outline-aux
              (cdr locs)
              (cons (list indentlvl (tuareg-utils-word-at-point charpos) 1 charpos) acc)
              indentlvl)))))))


;;;;; outline tree, the main data structure of an outline
(defun tuareg-outline-mktree (tree lvl list)
  (let* ((entry (car list))
         (curlvl (car entry))
         (curtype (nth 2 entry)))
    (cond
     ; done parsing
     ( (null list) (cons tree nil) )

     ; module
     ( (and (= curlvl lvl)
            (= curtype 0))
       (let* ((temp (tuareg-outline-mktree nil (+ lvl 2) (cdr list)))
              (subtree (car temp))
              (rest (cdr temp))
              (node (list 'module entry subtree)))
         (tuareg-outline-mktree (cons node tree) lvl rest)))

     ; let
     ( (and (= curlvl lvl)
            (= curtype 1))
       (tuareg-outline-mktree (cons (list 'let entry) tree) lvl (cdr list)))

     ; wrong lvl
     ( t (cons tree list)))))


(defun tuareg-outline-sort (tree)
  "sort the tree so that let expressions are before modules"
  (cond
   ( (null tree) tree)

   ( t (sort
        (pgu-list-foldl
         (lambda (acc node)
           (if (equal (car node) 'let) (cons node acc)
             (cons (list 'module
                         (nth 1 node)
                         (tuareg-outline-sort (nth 2 node)))
                   acc)))
         nil
         tree)
        (lambda (a b) (and (equal (car a) 'let)
                           (equal (car b) 'module)))))))


(defun tuareg-outline-make-outline ()
  "returns an outline of the current buffer.  Which is a list of
all the lines the outline should have."
  (let* ((locs (tuareg-outline-heading-locs))
         (rev-outline (tuareg-outline-make-outline-aux locs nil 0))
         (tree (car (tuareg-outline-mktree nil 0 (reverse rev-outline)))))
    (tuareg-outline-sort tree)))


(defun tuareg-outline-write-outline (tree)
  "writes out the outline tree"
  (pgu-list-iter
   (lambda (elem)
     (let* ((type (car elem))
            (entry (nth 1 elem))
            (str (nth 1 entry))
            (indentlvl (nth 0 entry))
            (subtree (nth 2 elem)))
       (cond
        ( (equal type 'module)
          (pgu-doNtimes (lambda () (insert "*")) indentlvl)
          (insert "* ")
          (insert str)
          (insert "\n")
          (tuareg-outline-write-outline subtree) )

        ( (equal type 'let)
          (pgu-doNtimes (lambda () (insert " ")) indentlvl)
          (insert "  ")
          (insert str)
          (insert "\n") ))))
   tree))




;;;;;; functions for operating on the produced outline view
(defun tuareg-outline-nth (n tree)
  (let* ((elem (car tree))
         (type (car elem))
         (entry (nth 1 elem))
         (subtree (nth 2 elem)))
    (cond
     ( (and (= n 0) (not (null tree)))
       (cons 'found (nth 1 elem)) )

     ( (null tree)
       (cons 'notfound n) )

     ( (equal type 'module)
       (let* ((temp (tuareg-outline-nth (- n 1) subtree))
              (foundp (equal (car temp) 'found))
              (arg (cdr temp)))
         (if foundp temp (tuareg-outline-nth arg (cdr tree)))))

     ( (equal type 'let)
       (tuareg-outline-nth (- n 1) (cdr tree))))))
     

(defun tuareg-outline-get-charpos-of-curline ()
  "grabs the charpos of this line in an outline buffer"
  (let* ((lineno (- (line-number-at-pos) 3))
         (index (- lineno 1)))
    (nth 3 (cdr (tuareg-outline-nth index tuareg-outline-base-outline)))))



(defun tuareg-outline-follow-entry (&optional usethiswindow)
  "goes to the definition of identifier (the line) at point"
  (interactive "P")
  (let ((charpos (tuareg-outline-get-charpos-of-curline))
        (base-buffer tuareg-outline-base-buffer))
    (if usethiswindow (switch-to-buffer tuareg-outline-base-buffer)
      (progn (other-frame 1)
             (switch-to-buffer base-buffer)))
    (goto-char charpos)
    (recenter)))




;;;;;;; main entry points
;;;###autoload
(defun tuareg-outline-create-outline ()
  "creates an outline of the current buffer.  If the outline
buffer already exists, it is overridden"
  (interactive)
  (let* ((outline (tuareg-outline-make-outline))
         (basebuffer (current-buffer))
         (basename (buffer-name basebuffer))
         (outline-buffer (get-buffer-create "*tuareg-outline*")))
    (when (not (string= basename "*caml-whatis*"))
      (save-excursion
        ;; setup buffer
        (set-buffer outline-buffer)
        (toggle-read-only -1)
        (erase-buffer)

        (insert basename) (insert "\n")
        (pgu-doNtimes (lambda () (insert "-")) (length basename))
        (insert "\n\n")
        (tuareg-outline-write-outline outline)

        (goto-char (point-min))
        (org-mode)
        (view-mode)
        (define-key view-mode-map "f" 'tuareg-outline-follow-entry)

        (make-local-variable 'tuareg-outline-base-buffer)
        (setq tuareg-outline-base-buffer basebuffer)
        (make-local-variable 'tuareg-outline-base-outline)
        (setq tuareg-outline-base-outline outline)))))



;;;###autoload
(defun tuareg-outline-create-outline-and-show ()
  "creates an outline of the current buffer.  The outline will be
shown in a buffer called *tuareg-outline-origname*.  If the buffer already
exists, it is overridden"
  (interactive)
  (tuareg-outline-create-outline)
  (switch-to-buffer (get-buffer "*tuareg-outline*")))



(provide 'tuareg-outline)
