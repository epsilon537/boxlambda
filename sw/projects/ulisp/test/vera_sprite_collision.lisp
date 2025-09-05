'start
(vera :display :enable)
(vera :sprites :enable)

(defvar w 64)
(defvar h 64)
(defvar bpp 8)
(vera :tileset 0 :width w :height h :bpp bpp :num_tiles 8)
(vera :tileset 0 :info)
(vera :sprite 1 :init)
(vera :sprite 1 :tileset 0 :tile 2)
(vera :sprite 1 :z VERA_SPRITE_Z_L1)
(vera :sprite 2 :init)
(vera :sprite 2 :tileset 0 :tile 2)
(vera :sprite 2 :z VERA_SPRITE_Z_L1)
(dotimes (yy h)
  (dotimes (xx (min w (1+ yy)))
    (vera_tileset_pixel 0 2 xx yy VERA_COLOR_CYAN)))












(vera :sprite 1 :x 40 :y 50)
(vera :sprite 2 :x 200 :y 200)
(vera :sprite 1 :mask 1)
(vera :sprite 2 :mask 1)
(vera :sprite 1 :mask)
(vera :sprite 2 :mask)
(vera :ien :set :mask VERA_IRQ_SPRCOL)
(vera :isr :get)

(vera :sprite 2 :x 40 :y 50)




(vera :isr :get)

'end
