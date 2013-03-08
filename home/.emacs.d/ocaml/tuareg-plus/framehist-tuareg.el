(define-key tuareg-mode-map "\C-c~" 'tuareg-next-camltop-error-withframehist)
(define-key tuareg-mode-map "\C-ct" 'tuareg-camltop-compile-buffer-withframehist)

(defun tuareg-camltop-compile-buffer-withframehist ()
  (interactive)
  (framehist:wpush)
  (tuareg-camltop-compile-buffer))

(defun tuareg-next-camltop-error-withframehist ()
  (interactive)
  (framehist:wpush)
  (tuareg-next-camltop-error))

(provide 'framehist-tuareg)
