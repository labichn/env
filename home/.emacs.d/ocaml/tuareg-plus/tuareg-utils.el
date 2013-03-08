(defun tuareg-utils-module-locs-aux (acc)
  (if (null (search-forward-regexp "^[ ]*module[ \n]+" nil t)) acc
    (tuareg-utils-module-locs-aux (cons (point) acc))))


(defun tuareg-utils-module-locs ()
  "returns a list of module locations (the locations returned is
at the beginning of the module name)"
  (save-excursion
    (goto-char (point-min))
    (tuareg-utils-module-locs-aux nil)))


(defun tuareg-utils-open-locs-aux (acc)
  (if (null (search-forward-regexp "^[ ]*open[ \n]+" nil t)) acc
    (tuareg-utils-open-locs-aux (cons (point) acc))))


(defun tuareg-utils-open-locs ()
  "returns a list of open locations (the locations returned is at
the begining of the module being opened"
  (save-excursion
    (goto-char (point-min))
    (tuareg-utils-open-locs-aux nil)))

(defun tuareg-utils-let-locs-aux (acc)
  (if (null (search-forward-regexp "^[ ]*\\(let rec\\|let\\)[ \n]+" nil t)) acc
    (tuareg-utils-let-locs-aux (cons (point) acc))))


(defun tuareg-utils-let-locs ()
  "returns a list of let locations (including let rec).
Locations are at the beginning of the identifier)"
  (save-excursion
    (goto-char (point-min))
    (tuareg-utils-let-locs-aux nil)))

(defun tuareg-utils-indent-level (&optional location)
  "returns the indentation level of the line at 'location' (which
is an int indicating character position).  The indentation level
is the number of spaces at the beginning of line"
  (save-excursion
    (goto-char (or location (point)))
    (let* ((start (progn (beginning-of-line) (point)))
           (end (search-forward-regexp "[ ]*")))
      (- end start))))

(defun tuareg-utils-word-at-point (&optional location)
  "returns the word at the point 'location'.  location must be at
the start of the word.  Works also for operators like (+)"
  (let ((location (or location (point))))
    (save-excursion
      (goto-char location)
      (if (equal (char-after) 40) ; if it's a open paren
          (buffer-substring location (progn (search-forward ")") (point)))
        (buffer-substring location (progn (search-forward-regexp "[ \n;]") (- (point) 1)))))))


(defun tuareg-module-name-of-buffer ()
  (pgu-string-capitalize-first-letter
   (file-name-sans-extension
    (file-name-nondirectory
     (buffer-file-name)))))

(provide 'tuareg-utils)