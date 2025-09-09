'start
(vera :display :enable)
(vera :layer 0 :enabled)
(vera :layer 1 :enabled)
(vera :map 0 :width 32 :height 32 :map-type +vera-map-type-tile+)
(vera :map 1 :width 32 :height 32 :map-type +vera-map-type-tile+)
(vera :tileset 0 :width 16 :height 16 :bpp 8 :num-tiles 32)
(vera :layer 0 :map 0)
(vera :layer 1 :map 1)
(vera :layer 0 :tileset 0)
(vera :layer 1 :tileset 0)
(vera :map 0 :entry :x 1 :y 1 :val 1)
(vera :map 1 :entry :x 1 :y 1 :val 2)

(dotimes (ii 16)
  (vera :pixel :tileset 0 :tile 1 :x 0 :y ii :val +vera-color-red+))

(dotimes (ii 16)
  (vera :pixel :tileset 0 :tile 2 :x ii :y 0 :val +vera-color-blue+))


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
