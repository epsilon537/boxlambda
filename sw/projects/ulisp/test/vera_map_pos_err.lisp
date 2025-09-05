'start
(defvar cols 32)
(defvar rows 32)
(vera :map 0 :width cols :height rows :map_type VERA_MAP_TYPE_TXT16)
(vera :map 0 :entry :x cols :y 0
      :val (logior (ash VERA_COLOR_BLUE 12) (ash VERA_COLOR_WHITE 8) 1))
(vera :map 0 :entry :x 0 :y rows
      :val (logior (ash VERA_COLOR_PURPLE 12) (ash VERA_COLOR_YELLOW 8) 1))
(vera :map 0 :deinit)

(setq cols 64)
(setq rows 64)
(vera :map 0 :width cols :height rows :map_type VERA_MAP_TYPE_TXT16)
(vera :map 0 :entry :x cols :y 0
      :val (logior (ash VERA_COLOR_BLUE 12) (ash VERA_COLOR_WHITE 8) 1))
(vera :map 0 :entry :x 0 :y rows
      :val (logior (ash VERA_COLOR_PURPLE 12) (ash VERA_COLOR_YELLOW 8) 1))
(vera :map 0 :deinit)

(setq cols 128)
(setq rows 128)
(vera :map 0 :width cols :height rows :map_type VERA_MAP_TYPE_TXT16)
(vera :map 0 :entry :x cols :y 0
      :val (logior (ash VERA_COLOR_BLUE 12) (ash VERA_COLOR_WHITE 8) 1))
(vera :map 0 :entry :x 0 :y rows
      :val (logior (ash VERA_COLOR_PURPLE 12) (ash VERA_COLOR_YELLOW 8) 1))
(vera :map 0 :deinit)

(setq cols 256)
(setq rows 256)
(vera :map 0 :width cols :height rows :map_type VERA_MAP_TYPE_TXT16)
(vera :map 0 :entry :x cols :y 0
      :val (logior (ash VERA_COLOR_BLUE 12) (ash VERA_COLOR_WHITE 8) 1))
(vera :map 0 :entry :x 0 :y rows
      :val (logior (ash VERA_COLOR_PURPLE 12) (ash VERA_COLOR_YELLOW 8) 1))
(vera :map 0 :deinit)

'end
