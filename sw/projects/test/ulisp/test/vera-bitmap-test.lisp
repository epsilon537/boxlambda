'start
(vera :init)
(dotimes (jj 2)
  (let ((idx 0))
    (dolist (wh (list (cons 320 32) (cons 640 16)))
      (dolist (bpp (list 1 2 4 8))
         (vera :tileset idx :width (car wh) :height (cdr wh) :bpp bpp :num-tiles 1)
         (print (vera :tileset idx :info))
         (setq idx (1+ idx)))))
  (dotimes (ii 8)
    (vera :tileset ii :deinit)))





'end

