'start
(vera :init)

(let ((idx 0))
  (dolist (wh (list (cons 32 256) (cons 256 32)))
    (vera :map idx :init :width (car wh) :height (cdr wh) :map_type (mod idx 3))
    (print (vera :map idx :info))
     (setq idx (1+ idx))))
(dotimes (ii 2)
  (vera :map ii :deinit))

'end

