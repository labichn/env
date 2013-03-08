;; Ian Johnson
;; http://lifeandmath.blogspot.com/2012/10/docs-for-racket-emacs.html
;; 2013-02-20

(setq raco-location "raco")
(require 'paredit)

(setq raco-doc-history nil)
(defun call-raco-doc (text)
  (shell-command (concat raco-location " doc -- '" text "' &")))

(defun get-current-word ()
  (paredit-handle-sexp-errors
   (save-excursion
    (progn (backward-sexp)
           (mark-sexp)
           (buffer-substring (mark) (point))))
    (save-excursion
      (progn (mark-sexp)
             (buffer-substring (mark) (point))))))
(defun raco-doc ()
  "call raco doc on selected text"
  (interactive)
  (call-raco-doc (get-current-word)))
(defun raco-doc-prompt ()
  "call raco doc on prompted text"
  (interactive)
  (let ((text (read-string (if (consp raco-doc-history)
                               (concat "raco doc [" (car raco-doc-history) "]: ")
                               "raco doc: ")
                           nil
                           'raco-doc-history
                           ;; default value should be most recent history item
                           (if (consp raco-doc-history)
                               (car raco-doc-history)
                               nil)
                           t))) ;; inherit the current input method
    (call-raco-doc text)))
(defun enable-raco-doc ()
  (define-key scheme-mode-map (kbd "C-c C-r") 'raco-doc)
  (define-key scheme-mode-map (kbd "C-c C-M-r") 'raco-doc-prompt))
(provide 'raco-doc)
