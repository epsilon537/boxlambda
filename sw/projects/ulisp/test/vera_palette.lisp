'start
(vera :display :enable)
(vera :layer 0 :enable)
(vera :tileset 0 :init :width 320 :height 32 :bpp 8 :num_tiles 1)
(vera :layer 0 :bitmap :tileset 0 :tile_idx 0)
(vera :tileset 0 :pixel :tile_idx 0 :x 0 :y 0 :val 255)
(print (vera :tileset 0 :pixel :tile_idx 0 :x 0 :y 0))

(vera :irqline 0)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 0))
(print (vera :linecapture :pixel :x 1))

(vera :palette :idx 0 :r 3 :g 2 :b 1)
(vera :palette :idx 255 :r 1 :g 2 :b 3)
(print (vera :palette :idx 0))
(print (vera :palette :idx 255))

(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 0))
(print (vera :linecapture :pixel :x 1))

(vera :palette :restore)
(print (vera :palette :idx 0))
(print (vera :palette :idx 255))

(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 0))
(print (vera :linecapture :pixel :x 1))

'end
