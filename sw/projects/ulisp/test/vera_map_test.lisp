'start
(vera_init)
(dotimes (jj 2)
  (let ((idx 0))
    (dolist (wh (list (cons 32 32) (cons 64 64) (cons 128 128)))
      (dolist (maptype (list 0 1 2))
         (vera_map idx (car wh) (cdr wh) maptype)
         (print (vera_map idx))
         (setq idx (1+ idx)))))
  (dotimes (ii 9)
    (vera_map_deinit ii)))
'end

