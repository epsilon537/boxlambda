'start
(defvar map_sz 32)
(defvar tile_sz 16)
(defvar sprite_sz 64)
(defvar x 128)
(defvar y 88)
(defvar bpp 4)
(defvar scale 0.5)
(defun vera_example ()
  (print "General setup...")
  (vera :display :enable)
  (vera :palette 0 :r 8 :g 8 :b 15)
  (vera :hscale scale)
  (vera :vscale scale)
  (vera :sprites :enable)
  (vera :tileset 0 :width tile_sz :height tile_sz :bpp bpp :num_tiles 1)
  (vera :tileset 1 :width sprite_sz :height sprite_sz :bpp bpp :num_tiles 1)
  (vera :map 0 :width map_sz :height map_sz :map_type VERA_MAP_TYPE_TILE)
  (vera :layer 0 :enable)
  (vera :layer 0 :map 0)
  (vera :layer 0 :tileset 0)
  (print "Initializing map...")
  (dotimes (ii map_sz)
    (dotimes (jj map_sz)
      (vera_map_entry 0 ii jj 0)
      #|(vera :map 0 :entry :x ii :y jj :val 0)|#))
  (print "Generating tile...")
  (dotimes (ii tile_sz)
    (vera :pixel :tileset 0 :tile 0 :x ii :y 0 :val VERA_COLOR_YELLOW)
    (vera :pixel :tileset 0 :tile 0 :x 0 :y ii :val VERA_COLOR_YELLOW)
    (vera :pixel :tileset 0 :tile 0 :x ii :y (1- tile_sz) :val VERA_COLOR_YELLOW)
    (vera :pixel :tileset 0 :tile 0 :x (1- tile_sz) :y ii :val VERA_COLOR_YELLOW))
  (print "Generating sprite tile...")
  (dotimes (ii sprite_sz)
    (dotimes (jj sprite_sz)
      (vera_tileset_pixel 1 0 ii jj VERA_COLOR_WHITE)
      #|(vera :tileset 1 :pixel :tile 0 :x ii :y jj :val VERA_COLOR_WHITE)|#))
  (print "Setting up sprite...")
  (vera :sprite 0 :init)
  (vera :sprite 0 :tileset 1 :tile 0)
  (vera :sprite 0 :z VERA_SPRITE_Z_L1)
  (vera :sprite 0 :x x :y y))




(vera_example)



<wait 10>


'end

