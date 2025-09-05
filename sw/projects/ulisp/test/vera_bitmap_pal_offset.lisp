'start
(vera :display :enable)
(vera :layer 0 :enable)
(dolist (bpp (list 2 4 8))
  (vera :tileset 0 :width 320 :height 32 :bpp bpp :num_tiles 1)
  (print (vera :tileset 0 :info))
  (vera :layer 0 :bitmap :tileset 0 :tile 0)
  (vera :layer 0 :pal_offset 0)
  (print (vera :layer 0 :pal_offset))
  (vera :tileset 0 :pixel :tile 0 :x 0 :y 0 :val 1)
  (vera :irqline 0)
  (vera :linecapture :enable)
  (loop (if (= (vera :linecapture :enabled) 0) (return)))
  (print (vera :linecapture :pixel :x 0))
  (print (vera :palette 1))
  (vera :layer 0 :pal_offset 1)
  (print (vera :layer 0 :pal_offset))
  (vera :linecapture :enable)
  (loop (if (= (vera :linecapture :enabled) 0) (return)))
  (print (vera :linecapture :pixel :x 0))
  (print (vera :palette 17))
  (vera :layer 0 :pal_offset 4)
  (print (vera :layer 0 :pal_offset))
  (vera :linecapture :enable)
  (loop (if (= (vera :linecapture :enabled) 0) (return)))
  (print (vera :linecapture :pixel :x 0))
  (print (vera :palette (1+ (* 4 16))))
  (vera :tileset 0 :deinit))





'end
