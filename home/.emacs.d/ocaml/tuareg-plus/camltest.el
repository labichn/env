(require 'framehistory)

(define-key tuareg-mode-map "\C-cd" 'tuareg-test-diff-res)
(define-key tuareg-mode-map "\C-c\C-d" 'tuareg-test-diff-res-this-buffer-before-point)
(define-key tuareg-interactive-mode-map "\C-cd" 'tuareg-test-diff-res)
(define-key tuareg-interactive-mode-map "\C-c\C-d" 'tuareg-test-diff-res-this-buffer-before-point)




(defun tuareg-test-diff-res ()
  (interactive)

  ;; save frame pos
  (framehist:wpush)

  ;; grab the bench and actual
  (destructuring-bind (bench actual)
      (save-excursion
        (other-window 1)
        (goto-char (point-max))
        (let* ((bb (progn (re-search-backward "^  Should be:") (+ (point) 12)))
               (be (progn (re-search-forward "^  But is:") (forward-line 0) (point)))
               (ab (+ be 9))
               (ae (progn (re-search-forward "^[<] ") (- (point) 2))))
          (list (buffer-substring bb be)
                (buffer-substring ab ae))))
                   
    ;; for debugging
    ;;(message "bench: %s" bench)
    ;;(message "actual: %s" actual)

    ;; create the two buffers and put the strings into them    
    (kill-buffer (get-buffer-create "*caml-testbench*"))
    (set-buffer (get-buffer-create "*caml-testbench*"))
    (insert (replace-regexp-in-string "[\\][n]" "\n" bench))
    
    (kill-buffer (get-buffer-create "*caml-testactual*"))
    (set-buffer (get-buffer-create "*caml-testactual*"))
    (insert (replace-regexp-in-string "[\\][n]" "\n" actual))

    (ediff-buffers "*caml-testactual*" "*caml-testbench*")))



(defun tuareg-test-diff-res-this-buffer-before-point ()
  (interactive)

  ;; grab the bench and actual
  (destructuring-bind (bench actual)
      (progn 
        (save-excursion
          (re-search-backward "^  But is:")
          (forward-line -1)
          (let* ((bb (+ (point) 1))
                 (be (- (progn (end-of-line) (point)) 1))
                 (ab (+ (progn (forward-line 1) (beginning-of-line) (point)) 1))
                 (ae (- (progn (end-of-line) (point)) 1)))
            (list (buffer-substring bb be)
                  (buffer-substring ab ae)))))
    
    ;; for debugging
    ;;(message "bench: %s" bench)
    ;;(message "actual: %s" actual)

    ;; create the two buffers and put the strings into them    
    (kill-buffer (get-buffer-create "*caml-testbench*"))
    (set-buffer (get-buffer-create "*caml-testbench*"))
    (insert (replace-regexp-in-string "[\\][n]" "\n" bench))
    
    (kill-buffer (get-buffer-create "*caml-testactual*"))
    (set-buffer (get-buffer-create "*caml-testactual*"))
    (insert (replace-regexp-in-string "[\\][n]" "\n" actual))

    (ediff-buffers "*caml-testactual*" "*caml-testbench*")))


(provide 'camltest)
