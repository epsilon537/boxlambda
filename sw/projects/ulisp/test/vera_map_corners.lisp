'start
(vera :display :enable)
(vera :tileset 0 :width 16 :height 16 :bpp 1 :num_tiles 32)

(dotimes (ii 16)
  (vera :pixel :tileset 0 :tile 1 :x ii :y ii :val 1)
  (vera :pixel :tileset 0 :tile 1 :x (- 15 ii) :y ii :val 1))

(vera :layer 0 :tileset 0)

(dolist (cols (list 32 64 128))
  (dolist (rows (list 32 64 128))
    (vera :map 0 :width cols :height rows :map_type VERA_MAP_TYPE_TXT16)
    (vera :layer 0 :map 0)
    (vera :map 0 :entry :x 0 :y 0 :val
                    (logior (ash VERA_COLOR_GREEN 12) (ash VERA_COLOR_WHITE 8) 1))
    (vera :map 0 :entry :x (1- cols) :y 0 :val
                    (logior (ash VERA_COLOR_BLUE 12) (ash VERA_COLOR_WHITE 8) 1))
    (vera :map 0 :entry :x 0 :y (1- rows) :val
                    (logior (ash VERA_COLOR_PURPLE 12) (ash VERA_COLOR_YELLOW 8) 1))
    (vera :map 0 :entry :x (1- cols) :y (1- rows) :val
                    (logior (ash VERA_COLOR_GREEN 12) (ash VERA_COLOR_GREY 8) 1))
    (format t "~x~%"  (vera :map 0 :entry :x 0 :y 0))
    (format t "~x~%" (vera :map 0 :entry :x (1- cols) :y 0))
    (format t "~x~%" (vera :map 0 :entry :x 0 :y (1- rows)))
    (format t "~x~%" (vera :map 0 :entry :x (1- cols) :y (1- rows)))
    (vera :map 0 :deinit 0)))




'end
