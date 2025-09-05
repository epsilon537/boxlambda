'start
(vera :display :enable)
(vera :tileset 0 :width 16 :height 16 :bpp 1 :num_tiles 32)

(dotimes (ii 16)
  (vera_tileset_pixel 0 1 ii ii 1)
  (vera_tileset_pixel 0 1 (- 15 ii) ii 1))








(vera :map 0 :width 32 :height 32 :map_type VERA_MAP_TYPE_TXT256)
(vera :layer 0 :map 0)
(vera :layer 0 :tileset 0)


(defvar row1 10)
(defvar col1 20)
(defvar row2 20)
(defvar col2 10)

(vera :map 0 :entry :x col1 :y row1 :val
                (logior (ash VERA_COLOR_GREEN 8) 1))
(vera :map 0 :entry :x col2 :y row2 :val
                (logior (ash VERA_COLOR_GRAYSCALE_10 8) 1))
(vera :layer 0 :enable)

(dolist (rowcol (list (cons row1 col1) (cons row2 col2)))
  (vera :irqline (* 16 (car rowcol)))
  (vera :linecapture :enable)
  (loop (if (= (vera :linecapture :enabled) 0) (return)))
  (dotimes (ii 16)
    (print (vera :linecapture :pixel :x (+ (* 16 (cdr rowcol)) ii)))))








'end
