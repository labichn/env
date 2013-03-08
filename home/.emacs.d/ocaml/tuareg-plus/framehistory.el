(require 'cl)

;; util funcs
(defun make-circular-list (alist)
  (let ((thelist (copy-list alist)))
    (setf (cdr (last thelist)) thelist)))

(defun make-circular-list-length (size)
  (let ((thelist nil))
    (do ((i 0))
        ((>= i size) (make-circular-list thelist))
      (setq thelist (cons nil thelist))
      (setf i (+ i 1)))))


;; yes, globals.  hurt me, I've been bad.
(defvar framehist:windowconf-tempcounter* 0)
(defvar framehist:windowconf-size* 1024)
(defvar framehist:windowconfs* 
  (make-circular-list-length framehist:windowconf-size*))


;; the functions
(defun framehist:wpush ()
  "Pushes a new frame configuration in to the history"
  (interactive)
  (let ((curr (list (current-window-configuration) (point-marker))))
    (if (not (equal curr (car framehist:windowconfs*)))
        (let ((nextpntr (cdr framehist:windowconfs*)))
          (message "pushing window configuration")
          ;; set that to the new window conf
          (setf (car nextpntr) curr)
          ;; set browser to start at new entry
          (setq framehist:windowconfs* nextpntr)
          ;; set the temp counter to zero because we've broken the
          ;; prev/next cycle
          (setq framehist:windowconf-tempcounter* 0))
      (message "same location, not pushing"))))

(defun framehist:wpush2 (window startpos)
  (framehist:wpush))

(defun framehist:wflush-reset ()
  "reset the frame history"
  (interactive)
  (setq framehist:windowconf-tempcounter* 0)
  (setq framehist:windowconf-size* 16)
  (setq framehist:windowconfs* 
        (make-circular-list-length framehist:windowconf-size*))
  (message "flushed"))

(defun framehist:temp-turn-off-hooks ()
  (remove-hook 'window-configuration-change-hook 'framehist:wpush)
  (remove-hook 'window-scroll-functions 'framehist:wpush2))

(defun framehist:temp-turn-on-hooks ()
  (add-hook 'window-configuration-change-hook 'framehist:wpush)
  (add-hook 'window-scroll-functions 'framehist:wpush2))

(defun framehist:wprev ()
  "go to the previous frame setup"
  (interactive)
  ;(framehist:temp-turn-off-hooks)
  (let* ((backupone (nthcdr (- framehist:windowconf-size* 1) framehist:windowconfs*))
         (prev (car backupone)))
    (if prev
        (progn
          (setq framehist:windowconfs* backupone)
          (set-window-configuration (car prev))
          (goto-char (cadr prev))
          (setq framehist:windowconf-tempcounter* 
                (- framehist:windowconf-tempcounter* 1))
          (message "%d" framehist:windowconf-tempcounter*))
      (message "End!")))
  ;(framehist:temp-turn-on-hooks)
  )


(defun framehist:wnext ()
  "go to the next frame"
  (interactive)
  ;(framehist:temp-turn-off-hooks)
  (let* ((forwardone (cdr framehist:windowconfs*))
         (next (car forwardone)))
    (if next
        (progn
          (setq framehist:windowconfs* forwardone)
          (set-window-configuration (car next))
          (goto-char (cadr next))
          (setq framehist:windowconf-tempcounter*
                (+ framehist:windowconf-tempcounter* 1))
          (message "%d" framehist:windowconf-tempcounter*))
      (message "No more!")))
  ;(framehist:temp-turn-on-hooks)
  )


(defun framehist:wpushprev ()
  "drop anchor (push) and then goto the previous frame configuration"
  (interactive)
  ;; if we're not in the middle of a prev/next chain, then push first
  ;; (to anchor where we were.  (but only do so if we're not already
  ;; at that achor)).  Otherwise just do standard prev/next
  (if (and (not (eq last-command 'framehist:wpushprev))
           (not (eq last-command 'framehist:wprev))
           (not (eq last-command 'framehist:wnext))
           (not (eq last-command 'handle-switch-frame)))
      (let ((curr (list (current-window-configuration) (point-marker))))
        (when (not (equal curr (car framehist:windowconfs*)))
          (framehist:wpush))
        (framehist:wprev))
    (framehist:wprev)))
  

;; key mappings
(global-set-key "\C-\M-p" 'framehist:wpushprev)
(global-set-key "\C-\M-n" 'framehist:wnext)
(define-key ctl-x-map "rh" 'framehist:wpush)


;; turn on automatic framehistory when window configuration changes
;; (add-hook 'window-configuration-change-hook 'framehist:wpush)

;; turn on automatic framehistory when window scrolls
;; (add-hook 'window-scroll-functions 'framehist:wpush2)

(provide 'framehistory)
