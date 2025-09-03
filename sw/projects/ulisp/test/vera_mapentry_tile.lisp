'start

(vera :display :enable)
(vera :tileset 0 :init :width 16 :height 16 :bpp 8 :num_tiles 32)

(dotimes (ii 16)
  (dotimes (jj (1+ ii))
    (vera_tileset_pixel 0 1 jj ii VERA_COLOR_BLUE)
    #|(vera :tileset 0 :pixel :tile 1 :x jj :y ii :val VERA_COLOR_BLUE)|#))









(vera :map 0 :init :width 32 :height 32 :map_type VERA_MAP_TYPE_TILE)
(vera :layer 0 :map 0)
(vera :layer 0 :tileset 0)

(defvar row1 10)
(defvar col1 20)
(defvar row2 20)
(defvar col2 10)
(defvar row3 16)
(defvar col3 16)

(vera :map 0 :entry :x col1 :y row1 :val 1)
(vera :map 0 :entry :x col2 :y row2 :val (logior VERA_MAPENTRY_HFLIP_MASK VERA_MAPENTRY_VFLIP_MASK 1))
(vera :map 0 :entry :x col3 :y row3 :val (logior (ash 1 VERA_MAPENTRY_PAL_OFFSET_SHIFT) 1))
(vera :layer 0 :enable)
(dolist (rowcol (list (cons row1 col1) (cons row2 col2) (cons row3 col3)))
  (vera :irqline (* 16 (car rowcol)))
  (vera :linecapture :enable)
  (loop (if (= (vera :linecapture :enabled) 0) (return)))
  (dotimes (ii 16)
    (print (vera :linecapture :pixel :x (+ (* 16 (cdr rowcol)) ii)))))





'end
