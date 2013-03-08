;; This file provides the ability to "compile" a buffer in the toplevel.  This
;; is just a glorified eval region, but fast, because it doesn't go through
;; comint.  It goes through a temporarily file which the ocaml toplevel is
;; directed to "#use".  It also interpretes of toplevel directives hidden in
;; comments

(require 'gen-utils)
(require 'framehistory)
(eval-when-compile (require 'tuareg-completion))
(eval-when-compile (require 'tuareg))

(define-key tuareg-mode-map "\C-ct" 'tuareg-camltop-compile-buffer-before-point)
(define-key tuareg-mode-map "\C-c\C-r" 'tuareg-camltop-eval-region)
(define-key tuareg-mode-map "\C-c~" 'tuareg-next-camltop-error)


(defvar tuareg-camltop-compile-tempfile "/tmp/_camltop-compile-tempfile.ml")
(defvar *tuareg-camltop-compile-lastpos* 0)


(defun tuareg-get-next-camltop-error-loc ()
  (save-excursion
    (set-buffer (get-buffer "*caml-toplevel*"))
    (let* ((begin (or *tuareg-camltop-compile-lastpos* (point)))
           (region (buffer-substring begin (point-max)))
           (errorstart (concat "File \".*" tuareg-camltop-compile-tempfile "\","))
           (mstart (string-match errorstart region)))
      (if mstart
          (destructuring-bind (junk rest) (pgu-string-split (substring region mstart) "line[ ]*")
            (destructuring-bind (linenum rest) (pgu-string-split rest "[0-9]+")
              (destructuring-bind (junk rest) (pgu-string-split rest "characters[ ]*")
                (destructuring-bind (charnum dontcare) (pgu-string-split rest "[0-9]+")
                  (setq *tuareg-camltop-compile-lastpos* 
                        (+ *tuareg-camltop-compile-lastpos* mstart 
                           (string-match "[\n]" (substring region mstart))))
                  (list (string-to-number linenum) (string-to-number charnum))))))
        nil))))


(defun tuareg-next-camltop-error ()
  (interactive)
  (framehist:wpush)
  (destructuring-bind (linenum charnum) (tuareg-get-next-camltop-error-loc)
    (goto-line-nowiden linenum)
    (beginning-of-line)
    (let* ((linebeg (point))
           (errorloc (+ linebeg charnum)))
      (goto-char errorloc))))

       


(defun tuareg-preprocess-camltop-compile-buffer-aux-loads ()
  (interactive)
  (if (search-forward-regexp "^[(][*][#]" nil t)
      (progn
        (beginning-of-line)
        (delete-char 2)
        (end-of-line)
        (delete-char -2)
        (tuareg-preprocess-camltop-compile-buffer-aux-loads))
    nil))


(defun tuareg-preprocess-camltop-compile-buffer-aux-modtypes ()
  (interactive)
  (if (search-forward-regexp "^module[ \n]+type" nil t)
      (progn
        (tuareg-makewrite-mod-from-modtype-at-point)
        (tuareg-preprocess-camltop-compile-buffer-aux-modtypes))
    nil))




(defun tuareg-preprocess-camltop-compile-buffer-aux-toplevel-ignores ()
  (interactive)
  (let ((beg (search-forward "(*toplevel-ignore-beg*)" nil t))
        (end (search-forward "(*toplevel-ignore-end*)" nil t)))
    (when beg
      (goto-char end)
      (insert "*)")
      (goto-char (- beg 23))
      (insert "(*")
      (goto-char end)
      (tuareg-preprocess-camltop-compile-buffer-aux-toplevel-ignores))))


;; removed "value:true"
;; (insert "#env \"module:false class:false\";; ")
;; (insert "\n\n#env \"module:true class:true\";;\n")
(defun tuareg-preprocess-camltop-compile-buffer ()
  (interactive)
  (goto-char (point-min))
  (tuareg-preprocess-camltop-compile-buffer-aux-loads)
  (goto-char (point-max))
  (let* ((startnonspacechar (search-backward-regexp "[^ \n][^ \n]"))
         (lasttwochar (buffer-substring-no-properties startnonspacechar (+ startnonspacechar 2))))
    (if (not (string= lasttwochar ";;")) 
        (progn
          (goto-char (point-max))
          (insert "\n;;\n")
          (goto-char (point-max)))
      (goto-char (point-max))))
  (goto-char (point-min))
  (tuareg-preprocess-camltop-compile-buffer-aux-toplevel-ignores))


(defun tuareg-camltop-compile-str (bufstr preprocessp)
  "Sends string to the toplevel via #use and a temp file"
  (save-excursion
    ;; make a temp file of the code we are passing to the toplevel
    (set-buffer (find-file-noselect tuareg-camltop-compile-tempfile))
    (delete-region (point-min) (point-max))
    (insert bufstr)
    (when preprocessp (tuareg-preprocess-camltop-compile-buffer))
    (save-buffer)
      
    ;; tell the toplevel to run that temp file
    (set-buffer (get-buffer tuareg-interactive-buffer-name))
    (goto-char (point-max))
    (setq *tuareg-camltop-compile-lastpos* (point))
    (comint-send-string 
     tuareg-interactive-buffer-name
     (concat "#use \"" tuareg-camltop-compile-tempfile "\";;"))
    (comint-send-input)
    (display-buffer tuareg-interactive-buffer-name)))


(defun tuareg-camltop-compile-buffer-before-point ()
  "Sends everything before point to the toplevel via #use and a temp file"
  (interactive)
  (framehist:wpush)
  (tuareg-camltop-compile-str (buffer-substring (point) (point-min)) t))

(defun tuareg-camltop-eval-region ()
  "Sends everything in region to toplevel"
  (interactive)
  (tuareg-camltop-compile-str (buffer-substring (region-beginning) (region-end)) t))

(provide 'camltop-compile)
