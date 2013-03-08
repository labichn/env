;;; paths
(setq load-path (cons "~/tuareg-plus" load-path))


;;; some useful functions
(defun reload-file ()
  (interactive)
  (let ((curpos (point))
        (file (buffer-file-name)))
    (kill-this-buffer)
    (find-file file)
    (goto-char curpos)
    (recenter)))

(defun goto-line-nowiden (arg)
  "Goto line ARG, counting from line 1 at beginning of buffer."
  (interactive "NGoto line: ")
  (setq arg (prefix-numeric-value arg))
  (save-restriction
    (goto-char 1)
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- arg))
      (forward-line (1- arg)))))


;;; narrow stack
(require 'narrow-stack)
(narrow-stack-mode)


;;; framehistory
(require 'winner)
(winner-mode)
(setq ediff-quit-hook '(lambda () 
                         (ediff-cleanup-mess)
                         (winner-undo)))


;;; completion
(setq ocaml-info-prefix "ocaml-indx")


;;; hooks to load tuareg-plus
(add-hook 'tuareg-mode-hook
          '(lambda ()
             (require 'tuareg-plus)
             (require 'camltest)
             (require 'winner)
             (winner-mode 1)
             (setq compile-command "make")
             (tuareg-outline-create-outline)))
             
(add-hook 'tuareg-interactive-mode-hook
          '(lambda ()
             (require 'tuareg-plus)))
