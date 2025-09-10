'start
(vera :display :enable)
(vera :sprites :enable)
(defvar w 8)
(defvar h 8)
(defvar bpp 8)
(vera :tileset 0 :width w :height h :bpp bpp :num-tiles 8)
(vera :sprite 0 :tileset 0 :tile 2)
(vera :sprite 0 :z +vera-sprite-z-l1+)
(vera :sprite 63 :tileset 0 :tile 2)
(vera :sprite 63 :z +vera-sprite-z-l1+)
(dotimes (yy h)
  (dotimes (xx (min w (1+ yy)))
    (vera-tileset-pixel 0 2 xx yy +vera-color-cyan+)))


















(vera :sprite 0 :x 40 :y 50)
(vera :sprite 63 :x 40 :y 70)

(vera :irqline 50)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 39))
(print (vera :linecapture :pixel :x 40))

(vera :irqline 70)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 39))
(print (vera :linecapture :pixel :x 40))

'end
