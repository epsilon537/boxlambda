'start
(vera :display :enable)
(vera :layer 0 :enable)
(vera :map 0 :init :width 32 :height 32 :map_type VERA_MAP_TYPE_TXT16)
(vera :layer 0 :map 0)
(vera :map 0 :entry :x 0 :y 0 :val 257)
(dolist (w (list 8 16))
  (dolist (h (list 8 16))
    (vera :tileset 0 :init :width w :height h :bpp 1 :num_tiles 8)
    (vera :layer 0 :tileset 0)
    (vera :tileset 0 :pixel :tile 1 :x (- w 1) :y (- h 1) :val 1)
    (print (vera :tileset 0 :pixel :tile 1 :x (- w 1) :y (- h 1)))
    (vera :irqline (- h 1))
    (vera :linecapture :enable)
    (loop (if (= (vera :linecapture :enabled) 0) (return)))
    (print (vera :linecapture :pixel :x (- w 1)))
    (vera :tileset 0 :pixel :tile 1 :x (- w 1) :y (- h 1) :val 0)
    (vera :tileset 0 :deinit)))






'end
