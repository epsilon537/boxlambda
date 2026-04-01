'start
(defvar map-sz 32)
(defvar tile-sz 16)
(defvar sprite-sz 64)
(defvar x 128)
(defvar y 88)
(defvar bpp 4)
(defvar scale 0.5)
(defun vera-example ()
  (print "General setup...")
  (vera :display :enable)
  (vera :palette 0 :r 8 :g 8 :b 15)
  (vera :hscale scale)
  (vera :vscale scale)
  (vera :sprites :enable)
  (vera :tileset 0 :width tile-sz :height tile-sz :bpp bpp :num-tiles 1)
  (vera :tileset 1 :width sprite-sz :height sprite-sz :bpp bpp :num-tiles 1)
  (vera :map 0 :width map-sz :height map-sz :map-type +vera-map-type-tile+)
  (vera :layer 0 :enable)
  (vera :layer 0 :map 0)
  (vera :layer 0 :tileset 0)
  (print "Initializing map...")
  (dotimes (ii map-sz)
    (dotimes (jj map-sz)
      (vera-map-entry 0 ii jj 0)))
  (print "Generating tile...")
  (dotimes (ii tile-sz)
    (vera :pixel :tileset 0 :tile 0 :x ii :y 0 :val +vera-color-yellow+)
    (vera :pixel :tileset 0 :tile 0 :x 0 :y ii :val +vera-color-yellow+)
    (vera :pixel :tileset 0 :tile 0 :x ii :y (1- tile-sz) :val +vera-color-yellow+)
    (vera :pixel :tileset 0 :tile 0 :x (1- tile-sz) :y ii :val +vera-color-yellow+))
  (print "Generating sprite tile...")
  (dotimes (ii sprite-sz)
    (dotimes (jj sprite-sz)
      (vera-tileset-pixel 1 0 ii jj +vera-color-white+)))
  (print "Setting up sprite...")
  (vera :sprite 0 :init)
  (vera :sprite 0 :tileset 1 :tile 0)
  (vera :sprite 0 :z +vera-sprite-z-l1+)
  (vera :sprite 0 :x x :y y))




(vera-example)



<wait 10>


'end

