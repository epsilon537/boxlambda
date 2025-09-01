'start
(dotimes (jj 2)
  (let ((idx 0))
    (dolist (wh (list (cons 8 8) (cons 16 16) (cons 32 32) (cons 64 64)))
      (dolist (bpp (list 1 2 4 8))
        (vera :tileset idx :init :width (car wh) :height (cdr wh) :bpp bpp :num_tiles 8)
        (print (vera :tileset idx :info))
        (setq idx (1+ idx)))))
  (dotimes (ii 16)
    (vera :tileset ii :deinit)))

<wait 10>

(vera :tileset 0 :init :width 8 :height 8 :bpp 1 :num_tiles 1)
(vera :tileset 1 :init :width 8 :height 8 :bpp 1 :num_tiles 1023)

'end

