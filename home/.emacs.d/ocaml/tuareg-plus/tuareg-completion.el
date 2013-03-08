(require 'framehistory)

(define-key tuareg-mode-map [kp-enter] 'tuareg-complete-at-point)
(define-key tuareg-mode-map "\M-\C-m" 'tuareg-complete-at-point)
(define-key tuareg-mode-map "\e\r" 'tuareg-complete-at-point)
(define-key tuareg-mode-map "\C-cw" 'tuareg-whatis)
(define-key tuareg-mode-map "\C-c\C-w" 'tuareg-whatis-only)

(define-key tuareg-interactive-mode-map [kp-enter] 'tuareg-complete-at-point)
(define-key tuareg-interactive-mode-map "\M-\C-m" 'tuareg-complete-at-point)
(define-key tuareg-interactive-mode-map "\e\r" 'tuareg-complete-at-point)
(define-key tuareg-interactive-mode-map "\C-cw" 'tuareg-whatis)
(define-key tuareg-interactive-mode-map "\C-c\C-w" 'tuareg-whatis-only)


;;;;;;;;; Utils

(defun tuareg-wait-for-output ()
  "Wait for output of caml process.  Usually called after a sommand is
sent with tuareg-interactive-send-input-or-indent"
  (let ((last-input-end (if (boundp 'comint-last-input-end)
                            comint-last-input-end
                          (point-max)))
        (empty-prompt (concat comint-prompt-regexp "$")))
    (while (progn			
             (goto-char last-input-end)
             (not (re-search-forward empty-prompt nil t)))
      (accept-process-output (get-buffer-process tuareg-interactive-buffer-name)))))


(defun tuareg-get-tell (partial &optional only)
  "Given a string <partial> representing a (perhaps) partial
identifier, this function returns the raw ocaml #tellall output"
  (save-excursion
    (tuareg-run-process-if-needed)
    (set-buffer tuareg-interactive-buffer-name)
    (font-lock-mode -1)
    (undo-boundary)
    (save-excursion
      (goto-char (point-max))
      (let* ( ;; now let's do some work
             (lastprompt (+ (re-search-backward comint-prompt-regexp) 2))
             (NU (delete-region lastprompt (point-max)))
             (NU (goto-char (point-max)))
             (NU (insert (concat (if only "#tell \"" "#tellall \"") partial "\";;")))
             (endofinput (point-max))
             (NU (tuareg-interactive-send-input-or-indent))
             (NU (tuareg-wait-for-output))
             (endofoutput (progn (goto-char (point-max)) (forward-line -1) (end-of-line) (point)))
             (NU (goto-char (point-max)))
             (ans (buffer-substring endofinput endofoutput)))
        (primitive-undo 1 buffer-undo-list)
        (font-lock-mode 1)
        ans))))




;;;;;;;;; Completion

(defun tuareg-get-nonclass-completions-aux (acc line)
  "Parse this line of the tell output.  If it is a completion we
  care about, clean it up and put it in acc.  Otherwise just
  return acc"
  (if (null (string-match "^[^ \t]" line))
      ; not at the right indent level, ignore
      acc
    (progn
      ; throw away body
      (setq line (pgu-string-truncate "[ ]*[:=][ ]*" line))

      ; throw away header
      (setq line (pgu-string-eat
                  "\\(module type\\|class type\\|module\\|class\\|val\\|type\\)[ ]*"
                  line))

      ; throw away path
      (setq line (car (reverse (split-string line "[.]" t))))

      ; throw away any junk
      (setq line (pgu-string-truncate "[ ]" line))

      (cons line acc))))


(defun tuareg-get-nonclass-completions (partial)
  "Given a string <partial> representing a (perhaps) partial
identifier, this function returns a list of possible completions of
that identifier"
  (pgu-list-uniq-m
    (pgu-list-foldl
      'tuareg-get-nonclass-completions-aux
      nil
      (split-string (tuareg-get-tell partial) "[\n]" t))))


(defun tuareg-get-type-of (id)
  "gets the type of this value"
  (let ((rawsig (tuareg-get-tell id)))
    (destructuring-bind (junk thing sig-and-rest)
        (pgu-string-split2 rawsig (concat "[ \n]+val[ \n]+" id "[ \n]+[:][ \n]+"))
      (destructuring-bind (pest thing empty)
          (pgu-string-split2 sig-and-rest "[ \n]*[=][ \n]*[<]obj[>]$")
        (destructuring-bind (pest thetype empty)
          (pgu-string-split2 pest "[a-zA-Z0-9_.]+$")
          thetype)))))


(defun tuareg-get-typesig (type)
  "returns the type signature (as a string) of this type"
  (when (not (string= type ""))
    (let* ((rawmatches (tuareg-get-tell type))
           (temp (pgu-string-split rawmatches "[ \n]*[\n]+type[ \n]*"))
           (typesig-and-junk (cadr temp))
           (typesig (car (pgu-string-split typesig-and-junk "[>][ \n]*[\n]+")))) typesig)))


(defun classidp (id)
  (let* ((temp (pgu-string-split id "[#]")))
    (not (string= (car temp) ""))))


(defun tuareg-get-completions (partial)
  (if (classidp partial)
      (tuareg-get-class-completions partial)
    (tuareg-get-nonclass-completions partial)))


(defun tuareg-get-word-at-point (&optional wordre)
  "Grab the previous word (or partial) at point (moves the point
to after the word)"
  (interactive)
  (let ((ans
         (save-excursion
           (let* ((debug nil)
                  (wordre (or wordre "[^a-zA-Z0-9#_.-]"))
                  (newpos (re-search-backward wordre nil t))
                  (newpos (if newpos
                              (+ newpos 1)
                            (point-min)))
                  (oldpos (if (= newpos (point-min))
                              (re-search-forward wordre nil t)
                            (progn (re-search-forward wordre nil t)
                                   (re-search-forward wordre nil t))))
                  (oldpos (if oldpos
                              (- oldpos 1)
                            (point-max)))
                  ;; name is the partial thing we are trying to complete.
                  (name (buffer-substring newpos oldpos)))
             (if debug (momentary-string-display (concat "[" name "]") (point)))
             (list oldpos name)))))
    (goto-char (car ans))
    (cadr ans)))


;;;###autoload
(defun tuareg-complete-at-point ()
  "Complete the identifier at point"
  (interactive)
  (let* ((partialID (tuareg-get-word-at-point))
         (shortID ;; the last part of the ID.  ie. with out module names
          (let* ((matchshortid (string-match "[a-z0-9A-Z_]+$" partialID))
                 (shortid (if matchshortid
                              (match-string 0 partialID)
                            "")))
            shortid))
         (complist (tuareg-get-completions partialID))
         (comptable (mapcar (lambda (str) (cons str 0)) complist))
         (fullname (completing-read "complete: " comptable nil t shortID))
         (suffix (substring fullname (length shortID))))
    (insert suffix)))


(defun tuareg-parse-simple-sig (sig acc)
  "parses 'unit -> string' into its parts"
  (if (string= sig "")
      (list (reverse acc) "")
    (if (string-match "^[ \n]*[>][ \n]*" sig)
        (list (reverse acc) sig)
      (destructuring-bind (pre match post) (pgu-string-split2 sig "[a-zA-Z0-9_'`.]+")
        (if (and (string= pre "") (not (string= match "")))
            (tuareg-parse-simple-sig (cadr (pgu-string-split post "[ \n]*")) (cons match acc))
      (destructuring-bind (pre match post) (pgu-string-split2 sig "[?][a-zA-Z0-9_'`.]+[ \n]*[:]")
        (if (and (string= pre "") (not (string= match "")))
            (tuareg-parse-simple-sig (cadr (pgu-string-split post "[ \n]*")) (cons match acc))
          (destructuring-bind (pre match post) (pgu-string-split2 sig "[*()]")
            (if (and (string= pre "") (not (string= match "")))
                (tuareg-parse-simple-sig (cadr (pgu-string-split post "[ \n]*")) (cons match acc))
              (destructuring-bind (pre match post) (pgu-string-split2 sig "[-][>]")
                (if (and (string= pre "") (not (string= match "")))
                    (tuareg-parse-simple-sig (cadr (pgu-string-split post "[ \n]*")) (cons match acc))
                  (if (string-match "^[ \n]*[<][ \n]*" sig)
                      (destructuring-bind (objsig rest) (tuareg-parse-objsig-as-str
                                                         (cadr (pgu-string-split sig "[ \n]*")))
                        (tuareg-parse-simple-sig (cadr (pgu-string-split rest "[ \n]*")) 
                                                 (cons objsig acc)))
                    (destructuring-bind (pre match post) (pgu-string-split2 sig "[;]")
                      (if (and (string= pre "") (not (string= match "")))
                          (list (reverse acc) (cadr (pgu-string-split post "[ \n]*")))))))))))))))))


(defun tuareg-parse-objsig-parts (sig acc)
  "parses the parts of an object eg, '_test : unit -> string; tostring
: unit -> string >' into its composite parts (the methods)"
  ;; objend
  (if (string-match "^[ \n]*[>][ \n]*" sig)
      (list (reverse acc) sig)
    ;; a method and its sig
    (destructuring-bind (methodname separator rest)
        (pgu-string-split2 sig "[ \n]*[:][ \n]*")
      (if (not (string= methodname ""))
          (destructuring-bind (siglist rest) (tuareg-parse-simple-sig rest nil)
            (tuareg-parse-objsig-parts 
             rest 
             (cons (list methodname 
                         (pgu-list-foldr
                          (lambda (x y) (concat x " " y))
                          (pgu-list-flatten siglist) "")) 
                   acc)))
        (progn
          (message "Object signature ends prematurely! Object: %s" sig)
          (list (reverse acc) sig))))))


(defun tuareg-parse-objsig (sig)
  "parses an object, eg. '<hash : unit -> int; show : unit -> string>'
to its parts eg. (hash, unit->int)"
  (destructuring-bind (junk objstart rest)
      (pgu-string-split2 sig "[ \n]*[<][ \n]*")
    (if (not (string= objstart ""))
        (destructuring-bind (method-and-sigs endofobj) 
            (tuareg-parse-objsig-parts rest nil)
            (destructuring-bind (junk endofobj rest) 
              (pgu-string-split2 endofobj "^[ \n]*[>][ \n]*")
            (list method-and-sigs rest)))
      (progn
        (message "Error.  Object doesn't start with '<'.  Object: %s" sig)
        (list nil sig)))))


(defun tuareg-parse-objsig-as-str (sig)
  (destructuring-bind (methods-and-sigs rest) (tuareg-parse-objsig sig)
    (let* ((remaininglen (length rest))
           (objsigstr (substring sig 0 (- (length sig) remaininglen))))
      (list objsigstr rest))))



(defun tuareg-get-methods-in-class (class)
  "gets all methods in this class"
  (let* ((typesig (tuareg-get-typesig class))
         (justsig (cadr (pgu-string-split typesig "[ \n]*[=][ \n]*")))
         (parts (car (tuareg-parse-objsig justsig))))
    parts))


(defun tuareg-get-class-completions (partial)
  "gets the completions of a class method invokation 'sm#_'"
  (let* ((temp (pgu-string-split partial "[#]"))
         (id (car temp))
         (id (subseq id 0 (- (length id) 1)))
         (partial (cadr temp))
         (type (tuareg-get-type-of id)))
    (when type
      (let* ((fullcomplist (tuareg-get-methods-in-class type))
             (matchingcomps (mapcar 'car fullcomplist))
             (matchingcomps (pgu-list-filter
                               matchingcomps
                               (lambda (completion)
                                 (string-match-fixed partial completion)))))
        matchingcomps))))




(defun tuareg-kill-whatis-buffer ()
  (interactive)
  (let ((curpoint (point)))
    (set-buffer "*caml-whatis*")

    ;; fix 'q'
    (tuareg-interactive-mode)
    (local-set-key "q" 'self-insert-command)
    (tuareg-mode)
    (local-set-key "q" 'self-insert-command)

    (kill-buffer "*caml-whatis*")
    (goto-char curpoint)))


(defun tuareg-whatis2 (&optional showatpoint only)
  (framehist:wpush)
  (let* ((id (tuareg-get-word-at-point))
         (valofid 
          (if (classidp id)
              (let* ((temp (pgu-string-split id "[#]"))
                     (obj (car temp))
                     (obj (subseq obj 0 (- (length obj) 1)))
                     (method (cadr temp))
                     (type (tuareg-get-type-of obj))
                     (allmethods (tuareg-get-methods-in-class type))
                     (method-name-and-sig 
                      (pgu-list-filter 
                         allmethods
                         (lambda (completion)
                           (string= method (car completion)))))
                     (methodsig (cadr (car method-name-and-sig))))
                methodsig)
            (tuareg-get-tell id only))))
    (if showatpoint
        (progn
          (kill-new valofid)
          (momentary-string-display 
           (concat "\n\n----------------------------\n" 
                   valofid
                   "\n---------------------------\n\n")
           (point)))
      (let ((outputbuf (get-buffer-create "*caml-whatis*")))
        (set-buffer outputbuf)

        ;; 'q' quits
        (tuareg-interactive-mode)
        (local-set-key "q" 'tuareg-kill-whatis-buffer)
        (tuareg-mode)
        (local-set-key "q" 'tuareg-kill-whatis-buffer)

        ;; insert the stuff
        (insert valofid)
        (set-buffer-modified-p nil)
        (display-buffer outputbuf)))))

(defun tuareg-whatis (&optional showatpoint)
  (interactive "P")
  (tuareg-whatis2 showatpoint))

(defun tuareg-whatis-only (&optional showatpoint)
  (interactive "P")
  (tuareg-whatis2 showatpoint t))

(provide 'tuareg-completion)
