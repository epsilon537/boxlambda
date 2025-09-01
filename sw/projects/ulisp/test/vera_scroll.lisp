'start
(vera :display :enable)
(vera :layer 0 :enable)
(vera :layer 0 :enabled)
(vera :map 0 :init :width 32 :height 32 :map_type VERA_MAP_TYPE_TILE)
(vera :tileset 0 :init :width 16 :height 16 :bpp 8 :num_tiles 32)
(vera :layer 0 :map 0)
(vera :layer 0 :tileset 0)
(vera :map 0 :entry :x 2 :y 2 :val 1)
(dotimes (ii 16)
  (vera :tileset 0 :pixel :tile_idx 1 :x ii :y ii :val VERA_COLOR_WHITE)
  (vera :tileset 0 :pixel :tile_idx 1 :x (- 15 ii) :y ii :val VERA_COLOR_WHITE))




(vera :layer 0 :hscroll 16)
(vera :layer 0 :vscroll 16)
(vera :layer 0 :hscroll)
(vera :layer 0 :vscroll)

(vera :irqline 16)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(dotimes (ii 3)
  (print (vera :linecapture :pixel :x (+ ii 15))))


(vera :irqline 32)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(dotimes (ii 3)
  (print (vera :linecapture :pixel :x (+ ii 31))))



'end
