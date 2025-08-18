'start
(vera_init)
(dotimes (jj 2)
  (let ((idx 0))
    (dolist (wh (list (cons 320 32) (cons 640 16)))
      (dolist (bpp (list 1 2 4 8))
         (vera_tileset idx (car wh) (cdr wh) bpp 1)
         (print (vera_tileset idx))
         (setq idx (1+ idx)))))
  (dotimes (ii 8)
    (vera_tileset_deinit ii)))
'end

