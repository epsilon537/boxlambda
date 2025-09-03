'start
(vera :display :enable)
(vera :sprites :enable)
(defvar w 64)
(defvar h 64)
(defvar bpp 8)
(vera :tileset 0 :init :width w :height h :bpp bpp :num_tiles 8)
(vera :tileset 0 :info)
(vera :sprite 1 :init)
(vera :sprite 1 :tileset 0 :tile 2)
(vera :sprite 1 :z VERA_SPRITE_Z_L1)
(dotimes (yy h)
  (dotimes (xx (min w (1+ yy)))
    (vera_tileset_pixel 0 2 xx yy VERA_COLOR_CYAN)))


















(vera :sprite 1 :x 40 :y 50)
(vera :sprite 1 :x)
(vera :sprite 1 :y)

(vera :irqline 50)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 40))
(print (vera :linecapture :pixel :x 41))

(vera :irqline (+ 50 (-  h 1)))
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 40))
(print (vera :linecapture :pixel :x 41))
(print (vera :linecapture :pixel :x (+ 40 63)))
(print (vera :linecapture :pixel :x (+ 40 64)))

(vera :irqline (+ 50 h))
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 40))
(print (vera :linecapture :pixel :x 47))

'end
