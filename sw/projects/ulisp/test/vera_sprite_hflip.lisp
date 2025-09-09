'start
(vera :display :enable)
(vera :sprites :enable)

(defvar w 64)
(defvar h 64)
(defvar bpp 8)
(vera :tileset 0 :width w :height h :bpp bpp :num-tiles 8)
(vera :tileset 0 :info)
(vera :sprite 1 :init)
(vera :sprite 1 :tileset 0 :tile 2)
(vera :sprite 1 :z +vera-sprite-z-l1+)
(dotimes (yy h)
  (dotimes (xx (min w (1+ yy)))
    (vera-tileset-pixel 0 2 xx yy +vera-color-cyan+)))
















(vera :sprite 1 :hflip :enable)
(vera :sprite 1 :hflip :enabled)
(vera :irqline 0)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 63))
(print (vera :linecapture :pixel :x 62))

(vera :irqline (- h 1))
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 0))
(print (vera :linecapture :pixel :x 7))
(print (vera :linecapture :pixel :x (1- (min w h))))
(print (vera :linecapture :pixel :x (min w h)))

(vera :irqline h)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 0))
(print (vera :linecapture :pixel :x 7))
(print (vera :linecapture :pixel :x (1- (min w h))))
(print (vera :linecapture :pixel :x (min w h)))

'end
