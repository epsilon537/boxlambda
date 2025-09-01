'start

(vera :display :enable)
(vera :layer 0 :enable)
(vera :map 0 :init :width 32 :height 32 :map_type VERA_MAP_TYPE_TILE)
(vera :tileset 0 :init :width 16 :height 16 :bpp 8 :num_tiles 32)
(vera :layer 0 :map 0)
(vera :layer 0 :tileset 0)

(vera :map 0 :entry :x 0 :y 0 :val 1)
(dotimes (ii 16)
  (vera_tileset_pixel 0 1 ii ii VERA_COLOR_WHITE)
  (vera_tileset_pixel 0 1 (- 15 ii) ii VERA_COLOR_WHITE))

(vera :irqline 15)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))

(print (vera :linecapture :pixel :x 14))
(print (vera :linecapture :pixel :x 15))
(print (vera :linecapture :pixel :x 16))

(vera :hscale 0.5 :vscale 0.5)

(vera :hscale)
(vera :vscale)

(vera :irqline 31)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))

(print (vera :linecapture :pixel :x 29))
(print (vera :linecapture :pixel :x 30))
(print (vera :linecapture :pixel :x 31))
(print (vera :linecapture :pixel :x 32))

'end
