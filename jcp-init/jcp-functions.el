;; C-w kill word backwards when no region is set
(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word
    backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

(defun number-of-cores ()
  "Return the number of cores present in the system"
  (when (eq system-type 'darwin)
    (string-to-number
     (car (process-lines "sysctl" "-n" "hw.physicalcpu")))))

(provide 'jcp-functions)
