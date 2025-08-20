'start
(vera_init)
(dotimes (jj 2)
  (let ((idx 0))
    (dolist (wh (list (cons 8 8) (cons 16 16) (cons 32 32) (cons 64 64)))
      (dolist (bpp (list 1 2 4 8))
         (vera_tileset idx (car wh) (cdr wh) bpp 8)
         (print (vera_tileset idx))
         (setq idx (1+ idx)))))
  (dotimes (ii 16)
    (vera_tileset_deinit ii)))
(vera_tileset 0 8 8 1 1)
(vera_tileset 1 8 8 1 1023)
'end

