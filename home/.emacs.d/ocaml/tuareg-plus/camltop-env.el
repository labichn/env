;; Function to set the toplevel environment to that of a particular place in a
;; .ml file.  Note that this cannot be perfect.  When we open a module we get
;; the full module.  If there are open statements in the middle of the module
;; we open, we do not have the precision to say, these module elements come
;; before the open statement and these module elements come after.  We assume
;; the open statement comes at the beginning of the module, so we perform the
;; open statement first, and then open the module


(require 'gen-utils)
(require 'tuareg-utils)
(require 'camltop-compile)
(require 'tuareg)

(define-key tuareg-mode-map "\C-ce" 'tuareg-setup-environment)

(defvar tuareg-environment-prefix nil "prefix for auto deriving the toplevel environment")


;;;;;; type loc = int
;;;;;; type envType = 'module | 'open
;;;;;; type envName = string
;;;;;; type envIndentlvl = int
;;;;;; type envEntry = envType * loc * envName * envIndentlvl
;;;;;; type env = envEntry list


(defun tuareg-environment-above-point (&optional location)
  "returns the env above point.  Note this is not a proper
  environment so certain module definitions and opens that are
  hidden in other modules are still included.  This is just a
  list of all module/open lines above (before) point.  The
  environment is returned in order, by location"
  (save-excursion
    (let* ((location (or location (point)))
           (NU (ns-narrow-to-region (point-min) location))
           (mod-locs (tuareg-utils-module-locs))
           (open-locs (tuareg-utils-open-locs))
           (modfun (lambda (acc loc)
                     (goto-char loc)
                     (cons (list 'module
                                 loc
                                 (tuareg-utils-word-at-point)
                                 (tuareg-utils-indent-level))
                           acc)))
           (openfun (lambda (acc loc)
                      (goto-char loc)
                      (cons (list 'open
                                  loc
                                  (tuareg-utils-word-at-point)
                                  (tuareg-utils-indent-level))
                            acc))))
      (ns-widen)
      (sort (pgu-list-foldl openfun
                            (pgu-list-foldl modfun nil mod-locs)
                            open-locs)
            (lambda (a b) (< (nth 1 a) (nth 1 b)))))))



(defun tuareg-environment-proper-above-point-aux (acc curlvl initlvl env)
  "env is in reverse order (bottom up)"
  (if (null env) acc
    (let* ((entry (car env))
           (type (car entry))
           (lvl (nth 3 entry)))
      (if (or (and (equal type 'module)
                   (<= lvl curlvl)
                   (< lvl initlvl))
              (and (equal type 'open)
                   (<= lvl curlvl)))
          (tuareg-environment-proper-above-point-aux (cons entry acc)
                                                     lvl
                                                     initlvl
                                                     (cdr env))
        (tuareg-environment-proper-above-point-aux acc
                                                   curlvl
                                                   initlvl
                                                   (cdr env))))))
  

(defun tuareg-environment-proper-above-point (&optional location)
  "returns the env above point.  It is a proper env which means
the indent level is monotonically non-decreasing.  The indent
level starts from the current indent lvl of phrase at point.  It
is also in order by location"
  (let* ((envraw (reverse (tuareg-environment-above-point location)))
         (filemodule (tuareg-module-name-of-buffer))
         (lvl (tuareg-utils-indent-level)))
    (cons (list 'module 0 filemodule -2)
          (tuareg-environment-proper-above-point-aux
            nil
            lvl
            lvl
            envraw))))


(defun tuareg-environment-opens-above-point ()
  (interactive)
  (reverse
   (pgu-list-foldl
    (lambda (acc entry)
      (if (equal (nth 0 entry) 'open) (cons (nth 2 entry) acc) acc))
    nil
    (tuareg-environment-proper-above-point))))


(defun tuareg-linearize-environment-aux (acc env trglvl)
  "first argument is the accumulated linear version, second
argument is the proper environment, the third is the target
module lvl we are interested in."
  (let* ((entry (car env))
         (type (nth 0 entry))
         (name (nth 2 entry))
         (lvl (nth 3 entry))
         (lvl2 (nth 3 (cadr env))))

    (cond
     ( (null entry) (reverse acc))

     ( (and (equal type 'module)
            (= lvl trglvl)
            (or (null lvl2)
                (not (= lvl lvl2))))
       (tuareg-linearize-environment-aux
          (cons (cons type name) acc)
          (cdr env)
          lvl2))
     
     ( (equal type 'open)
       (tuareg-linearize-environment-aux
          (cons (cons type name) acc)
          (cdr env)
          trglvl))

     ( t
       (tuareg-linearize-environment-aux
          acc
          (cdr env)
          trglvl)))))

(defun tuareg-linearize-environment (env)
  "turns a proper environment into a linearized one (eg. remove
sibling module levels)"
  (tuareg-linearize-environment-aux nil env -2))


(defun tuareg-mk-environment-aux-add-opens (acc env)
  (let* ((entry (car env))
         (type (car entry))
         (name (cdr entry)))
    (cond
     ( (or (null entry)
           (not (equal type 'open)))
       (cons acc env))

     ( (equal type 'open)
       (tuareg-mk-environment-aux-add-opens
         (cons name acc)
         (cdr env))))))


(defun tuareg-mk-environment-aux (acc linenv)
  (let* ((entry (car linenv))
         (type (car entry))
         (name (cdr entry)))
    (cond
     ( (null entry)
       (reverse acc))

     ( (equal type 'module)
       (let ((res (tuareg-mk-environment-aux-add-opens
                   (cons name acc)
                   (cdr linenv))))
         (tuareg-mk-environment-aux
           (cons name (car res))
           (cdr res))))
     
     ( (equal type 'open)
       (tuareg-mk-environment-aux (cons name acc) (cdr linenv))))))


(defun tuareg-mk-environment (linenv)
  "makes the environment from a linearized environment.  This
transformation is mainly putting the opens before their
containing modules"
  (tuareg-mk-environment-aux nil linenv))


;;;###autoload
(defun tuareg-setup-environment ()
  (interactive)
  (let* ((env (tuareg-mk-environment
               (tuareg-linearize-environment
                (tuareg-environment-proper-above-point))))
         (envlines (mapcar (lambda (name) (concat "open " name)) env))
         (envtext (pgu-string-of-list tuareg-environment-prefix ";;\n" ";;\n" envlines)))
    (tuareg-camltop-compile-str envtext nil)))



(defun tuareg-set-environment-prefix ()
  (interactive)
  (setq tuareg-environment-prefix (read-string "environment prefix: ")))



(provide 'camltop-env)
