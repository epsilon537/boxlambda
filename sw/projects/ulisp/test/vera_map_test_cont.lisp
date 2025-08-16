'start
(vera_init)
(let ((idx 0))
  (dolist (wh (list (cons 32 256) (cons 256 32)))
     (vera_map idx (car wh) (cdr wh) (mod idx 3))
     (print (vera_map idx))
     (setq idx (1+ idx))))
(dotimes (ii 2)
  (vera_map_deinit ii))
'end

