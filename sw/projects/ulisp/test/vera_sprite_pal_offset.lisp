'start
(vera :display :enable)
(vera :sprites :enable)

(defvar w 64)
(defvar h 64)
(defvar bpp 8)
(vera :tileset 0 :init :width w :height h :bpp bpp :num_tiles 4)
(vera :tileset 0 :info)
(vera :sprite 1 :init)
(vera :sprite 1 :tileset 0 :tile_idx 2)
(vera :sprite 1 :z VERA_SPRITE_Z_L1)
(dotimes (yy h)
  (dotimes (xx (min w (1+ yy)))
    (vera_tileset_pixel 0 2 xx yy VERA_COLOR_CYAN)))


















(vera :irqline 0)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 0))

(vera :sprite 1 :pal_offset 1)
(vera :sprite 1 :pal_offset)

(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 0))

'end
