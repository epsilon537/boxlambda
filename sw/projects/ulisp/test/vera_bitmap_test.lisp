'start
(vera_init)
(dotimes (jj 2)
  (let ((idx 0))
    (dolist (wh (list (cons 320 32) (cons 640 16)))
      (dolist (bpp (list 1 2 4 8))
         (vera_bitmap idx (car wh) (cdr wh) bpp)
         (print (vera_bitmap idx))
         (setq idx (1+ idx)))))
  (dotimes (ii 8)
    (vera_bitmap_deinit ii)))
'end

