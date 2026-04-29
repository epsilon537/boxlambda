'start
(vera :display :enable)
(vera :tileset 0 :width 16 :height 16 :bpp 1 :num-tiles 32)

(dotimes (ii 16)
  (vera :pixel :tileset 0 :tile 1 :x ii :y ii :val 1)
  (vera :pixel :tileset 0 :tile 1 :x (- 15 ii) :y ii :val 1))

(vera :layer 0 :tileset 0)

(let ((cols (list 256 256 256 32 64 128))
      (rows (list 32 64 128 256 256 256)))
  (mapc (lambda (r c)
      (vera :map 0 :width c :height r :map-type +vera-map-type-txt16+)
      (vera :layer 0 :map 0)
      (vera :map 0 :entry :x 0 :y 0 :val
                      (logior (ash +vera-color-green+ 12) (ash +vera-color-white+ 8) 1))
      (vera :map 0 :entry :x (1- c) :y 0 :val
                      (logior (ash +vera-color-blue+ 12) (ash +vera-color-white+ 8) 1))
      (vera :map 0 :entry :x 0 :y (1- r) :val
                      (logior (ash +vera-color-purple+ 12) (ash +vera-color-yellow+ 8) 1))
      (vera :map 0 :entry :x (1- c) :y (1- r) :val
                      (logior (ash +vera-color-green+ 12) (ash +vera-color-grey+ 8) 1))
      (format t "~x~%"  (vera :map 0 :entry :x 0 :y 0))
      (format t "~x~%" (vera :map 0 :entry :x (1- c) :y 0))
      (format t "~x~%" (vera :map 0 :entry :x 0 :y (1- r)))
      (format t "~x~%" (vera :map 0 :entry :x (1- c) :y (1- r)))
      (vera :map 0 :deinit 0))
    rows cols))




'end
