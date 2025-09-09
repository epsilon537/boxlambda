'start
(defvar cols 32)
(defvar rows 32)
(vera :map 0 :width cols :height rows :map-type +vera-map-type-txt16+)
(vera :map 0 :entry :x cols :y 0
      :val (logior (ash +vera-color-blue+ 12) (ash +vera-color-white+ 8) 1))
(vera :map 0 :entry :x 0 :y rows
      :val (logior (ash +vera-color-purple+ 12) (ash +vera-color-yellow+ 8) 1))
(vera :map 0 :deinit)

(setq cols 64)
(setq rows 64)
(vera :map 0 :width cols :height rows :map-type +vera-map-type-txt16+)
(vera :map 0 :entry :x cols :y 0
      :val (logior (ash +vera-color-blue+ 12) (ash +vera-color-white+ 8) 1))
(vera :map 0 :entry :x 0 :y rows
      :val (logior (ash +vera-color-purple+ 12) (ash +vera-color-yellow+ 8) 1))
(vera :map 0 :deinit)

(setq cols 128)
(setq rows 128)
(vera :map 0 :width cols :height rows :map-type +vera-map-type-txt16+)
(vera :map 0 :entry :x cols :y 0
      :val (logior (ash +vera-color-blue+ 12) (ash +vera-color-white+ 8) 1))
(vera :map 0 :entry :x 0 :y rows
      :val (logior (ash +vera-color-purple+ 12) (ash +vera-color-yellow+ 8) 1))
(vera :map 0 :deinit)

(setq cols 256)
(setq rows 256)
(vera :map 0 :width cols :height rows :map-type +vera-map-type-txt16+)
(vera :map 0 :entry :x cols :y 0
      :val (logior (ash +vera-color-blue+ 12) (ash +vera-color-white+ 8) 1))
(vera :map 0 :entry :x 0 :y rows
      :val (logior (ash +vera-color-purple+ 12) (ash +vera-color-yellow+ 8) 1))
(vera :map 0 :deinit)

'end
