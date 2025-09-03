'start
(vera :display :enable)
(vera :sprites :enable)

(defvar w 8)
(defvar h 8)
(defvar bpp 8)

(vera :tileset 0 :init :width w :height h :bpp bpp :num_tiles 8)
(vera :tileset 0 :info)
(vera :sprite 1 :init)
(vera :sprite 65 :init)
(vera :sprite 1 :tileset 0 :tile 1)
(vera :sprite 65 :tileset 0 :tile 1)
(vera :sprite 65 :pal_offset 1)
(vera :sprite 1 :z VERA_SPRITE_Z_L1)
(vera :sprite 65 :z VERA_SPRITE_Z_L1)
(vera :tileset 0 :pixel :tile 1 :x 0 :y 0 :val VERA_COLOR_CYAN)

(vera :sprite 1 :x 40 :y 50)
(vera :sprite 65 :x 100 :y 200)

(vera :irqline 50)
(vera :spritebank :select 0)
(vera :spritebank :selected)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 40))

(vera :irqline 200)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 100))

(vera :irqline 50)
(vera :spritebank :select 1)
(vera :spritebank :selected)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 40))

(vera :irqline 200)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(print (vera :linecapture :pixel :x 100))

'end
