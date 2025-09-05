'start
(vera :display :enable)
(vera :layer 0 :enabled)
(vera :layer 1 :enabled)
(vera :map 0 :width 32 :height 32 :map_type VERA_MAP_TYPE_TILE)
(vera :map 1 :width 32 :height 32 :map_type VERA_MAP_TYPE_TILE)
(vera :tileset 0 :width 16 :height 16 :bpp 8 :num_tiles 32)
(vera :layer 0 :map 0)
(vera :layer 1 :map 1)
(vera :layer 0 :tileset 0)
(vera :layer 1 :tileset 0)
(vera :map 0 :entry :x 1 :y 1 :val 1)
(vera :map 1 :entry :x 1 :y 1 :val 2)

(dotimes (ii 16)
  (vera :pixel :tileset 0 :tile 1 :x 0 :y ii :val VERA_COLOR_RED))

(dotimes (ii 16)
  (vera :pixel :tileset 0 :tile 2 :x ii :y 0 :val VERA_COLOR_BLUE))


(vera :irqline 16)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(vera :linecapture :pixel :x 16)
(vera :layer 0 :enable)
(vera :layer 0 :enabled)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(vera :linecapture :pixel :x 16)

(vera :layer 1 :enable)
(vera :layer 1 :enabled)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(vera :linecapture :pixel :x 16)

'end
