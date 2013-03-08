;;;;;; list functions
(defun pgu-list-take-aux (p l a)
  (if (null l) (reverse a)
    (if (funcall p (car l))
        (pgu-list-take-aux p (cdr l) (cons (car l) a))
      (reverse a))))

(defun pgu-list-take (p l)
  "given a list [l], it will return the first sequence of elements
  that meet the predicate [p]"
  (pgu-list-take-aux p l nil))

(defun pgu-list-cons-if-new (x l)
  (if (memq x l) l (cons x l)))

(defun pgu-list-init (n f i acc)
  "list-init 10 id 0 nil creates [9 .. 0]"
  (if (= i n) acc (pgu-list-init n f (+ i 1) (cons (funcall f i) acc))))

(defun pgu-list-flatten (lis)
  "Removes nestings from a list."
  (cond ((atom lis) lis)
        ((eq (car lis) nil)
         (append '(nil) (pgu-list-flatten (cdr lis))))
        ((listp (car lis))
         (append (pgu-list-flatten (car lis)) 
                 (pgu-list-flatten (cdr lis))))
        (t (append (list (car lis)) 
                   (pgu-list-flatten (cdr lis))))))

(defun pgu-list-assoc-all (x l)
  "gets *all* the elements associated with this key"
  (pgu-list-foldl (lambda (acc e) (if (equal (car e) x) (cons e acc) acc))
         nil l))

(defun pgu-list-foldr (fun list base)
  (if (null list) base
    (funcall fun (car list) (pgu-list-foldr fun (cdr list) base))))

(defun pgu-list-foldl (fun base list)
  (if (null list) base
    (pgu-list-foldl fun (funcall fun base (car list)) (cdr list))))

(defun pgu-list-take-while (pre post pred)
  (if (or (null post)
          (not (funcall pred (car post))))
      (cons (reverse pre) post)
    (pgu-list-take-while (cons (car post) pre) (cdr post) pred)))

(defun pgu-list-iter (f l)
  (when (not (null l))
    (funcall f (car l))
    (pgu-list-iter f (cdr l))))

(defun pgu-list-filter (p l)
  "returns a list with only the elements in alist that we want"
  (pgu-list-foldl (lambda (acc x) (if (funcall p x) (cons x acc) acc)) nil l))

(defun pgu-list-exists (x list)
  (if (null list) nil
    (or (equal (car list) x) (pgu-list-exists x (cdr list)))))

(defun pgu-list-remove-elem-m (l)
  "removes the 2nd element in list [l].  Eg. if [l] is (1 2 3) then (1 3) is returned"
  (setcdr l (cdr (cdr l)))
  nil)

(defun pgu-list-uniq-aux (ht prevlist curlist)
  (when (not (null curlist))
    (if (null (gethash (car curlist) ht))
        (progn (pgu-list-remove-elem-m prevlist)
               (pgu-list-uniq-aux ht prevlist (cdr prevlist)))
      (progn (remhash (car curlist) ht)
             (pgu-list-uniq-aux ht curlist (cdr curlist))))))

(defun pgu-list-uniq-m (list)
  "uniqifies the list.  it is destructive.  it is stable"
  (let ((ht (make-hash-table :test 'equal :weakness t
                             :size (/ (* (length list) 3) 2))))
    (pgu-list-iter (lambda (x) (puthash x t ht)) list)
    (pgu-list-uniq-aux ht list (cdr list))
    list))

(defun pgu-list-uniq-on-sorted (lst)
  "uniqifies the list assuming it's sorted"
  (reverse (pgu-list-foldl (lambda (acc x)
                             (if (null acc) (cons x acc)
                               (if (equal x (car acc)) acc
                                 (cons x acc))))
                           nil
                           lst)))


;;;;;; string functions
(defun pgu-string-to-number (str)
  "like string-to-number but more robust.  ie. ignores non-number characters"
  (when (string-match "[^-0-9]*\\([-0-9]+\\)" str)
    (string-to-number (match-string 1 str))))

(defun pgu-subseq (seq start &optional end)
  "like subseq but more robust.  If the end is past the length of
the sequence it just returns the whole sequence"
  (if end
      (subseq seq start (min end (length seq)))
    (subseq seq start)))

(defun string-match-fixed (partial full)
  "checks if full starts with partial.  So 'h' and 'hello' is true but
'e' and 'hello' is false"
  (string= partial (substring full 0 (length partial))))

(defun pgu-string-split (str regex)
  "splits based on first match of regexp in str.  So 'hello' and 'e'
returns 'he' and 'llo'"
  (let ((amatch (string-match regex str)))
    (if amatch
        (let ((end (match-end 0)))
          (list (substring str 0 end) (substring str end)))
      (list "" str))))

(defun pgu-string-split2 (str regex)
  "splits based on first match of regexp in str.  So 'hello' and 'e'
returns 'h', 'e' and 'llo'"
  (let ((amatch (string-match regex str)))
    (if amatch
        (let ((end (match-end 0)))
          (list (substring str 0 amatch) (substring str amatch end) (substring str end)))
      (list "" "" str))))

(defun pgu-string-strip-trailing-spaces (str)
  (car (split-string str "[ ]+$")))

(defun pgu-string-of-list (pre post sep slist)
  "converts the list into a string."
  (let* ((mid (pgu-list-foldl (lambda (acc x) (concat acc sep x)) "" slist))
         (mid (when (not (equal mid "")) (subseq mid (length sep)))))
    (concat pre mid post)))

(defun pgu-string-replace (str old new newacc)
  "does a fixed replacement in string [str].  [newacc] is the prefix for the return string"
  (destructuring-bind (pre mid post) (pgu-string-split2 str old)
    (cond
     ((string= mid "") (pgu-string-of-list "" "" new (reverse (cons post newacc))))
     (t (pgu-string-replace post old new (cons pre newacc))))))

(defun pgu-string-eat (regexp str)
  "Eats (removes) the regexp match and every before it Eg. 'e'
and 'hello' yield 'llo'"
  (cadr (pgu-string-split str regexp)))

(defun pgu-string-truncate (regexp str)
  "truncates the string by removing the matching regexp and
everything after it.  Eg. 'e' and 'hello' yield 'h'"
  (if (string= str "") ""
    (let ((ans (car (pgu-string-split2 str regexp))))
      (if (string= ans "") str ans))))

(defun pgu-string-capitalize-first-letter (str)
  (if (> (length str) 0)
      (concat (capitalize (substring str 0 1)) (substring str 1))
    str))




;;;;; control
(defun pgu-foldNtimes (n f i acc)
  "(foldNtimes 3 (lambda (x) (concat x \"i\")) 0 \"\") yields \"iii\""
  (if (= i n) acc (pgu-foldNtimes n f (+ i 1) (funcall f acc))))

(defun pgu-doNtimes (f n)
  (when (> n 0) (funcall f) (pgu-doNtimes f (- n 1))))


(defun pgu-run-with-exception-handling (f fargs ef efargs)
  (condition-case nil (apply f fargs) (error (apply ef efargs))))

(defmacro pgu-do-when-mode-defined (modename do-name e^)
   (let ((mode-map-name (intern (format "%s-mode-map" modename)))
	 (mode-hook-name (intern (format "%s-mode-hook" modename)))
	 (fun-name (intern (format "define-%s-%s" do-name modename))))
      `(cond ((and (boundp ',mode-map-name)
		   ,mode-map-name)
              (setq defining* t)
	      ,e^)
	     (t
	      (defun ,fun-name ()
		 ,e^
		 (remove-hook ',mode-hook-name ',fun-name))
	      (add-hook ',mode-hook-name ',fun-name)))))


;;;;;; buffer manipulation
(defun pgu-get-line (linenum)
  "gets the specified line in the buffer and returns it as a string.
Lines are 1 indexed"
  (interactive "p")
  (save-excursion
    (goto-line linenum)
    (let* ((beg (point))
           (end (progn (end-of-line) (point))))
      (buffer-substring-no-properties beg end))))

(defun pgu-blank-line (linenum)
  "blanks out the line in the buffer.  Makes it empty"
  (save-excursion
    (goto-line linenum)
    (let ((beg (point))
          (end (progn (end-of-line) (point))))
      (delete-region beg end))))


(defun pgu-get-indent-at-point ()
  "returns the indentation of this line (an int)"
  (interactive)
  (save-excursion
    (let* ((beg (progn (forward-line 0) (point)))
           (end (progn (re-search-forward "[^ \t]") (- (point) 1)))
           (res (- end beg)))
      (message "%s" res)
      res)))


;;;;;; math functions
(defun pgu-abs (number)
  "like abs but if the number if nil, it will return nil"
  (when number (abs number)))


(provide 'gen-utils)
