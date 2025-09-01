'start

(vera :display :enable)
(vera :layer 0 :enable)
(vera :tileset 0 :init :width 320 :height 32 :bpp 8 :num_tiles 1)
(vera :layer 0 :bitmap :tileset 0 :tile_idx 0)

(vera_tileset_pixel 0 0 0 0 VERA_COLOR_BLACK)
(vera_tileset_pixel 0 0 1 0 VERA_COLOR_WHITE)
(vera_tileset_pixel 0 0 2 0 VERA_COLOR_RED)
(vera_tileset_pixel 0 0 3 0 VERA_COLOR_CYAN)
(vera_tileset_pixel 0 0 4 0 VERA_COLOR_PURPLE)
(vera_tileset_pixel 0 0 5 0 VERA_COLOR_GREEN)
(vera_tileset_pixel 0 0 6 0 VERA_COLOR_BLUE)
(vera_tileset_pixel 0 0 7 0 VERA_COLOR_YELLOW)
(vera_tileset_pixel 0 0 8 0 VERA_COLOR_ORANGE)
(vera_tileset_pixel 0 0 9 0 VERA_COLOR_BROWN)
(vera_tileset_pixel 0 0 10 0 VERA_COLOR_LIGHT_RED)
(vera_tileset_pixel 0 0 11 0 VERA_COLOR_DARK_GREY)
(vera_tileset_pixel 0 0 12 0 VERA_COLOR_GREY)
(vera_tileset_pixel 0 0 13 0 VERA_COLOR_LIGHT_GREEN)
(vera_tileset_pixel 0 0 14 0 VERA_COLOR_LIGHT_BLUE)
(vera_tileset_pixel 0 0 15 0 VERA_COLOR_LIGHT_GREY)









(vera_tileset_pixel 0 0 16 0 VERA_COLOR_GRAYSCALE_0)
(vera_tileset_pixel 0 0 17 0 VERA_COLOR_GRAYSCALE_1)
(vera_tileset_pixel 0 0 18 0 VERA_COLOR_GRAYSCALE_2)
(vera_tileset_pixel 0 0 19 0 VERA_COLOR_GRAYSCALE_3)
(vera_tileset_pixel 0 0 20 0 VERA_COLOR_GRAYSCALE_4)
(vera_tileset_pixel 0 0 21 0 VERA_COLOR_GRAYSCALE_5)
(vera_tileset_pixel 0 0 22 0 VERA_COLOR_GRAYSCALE_6)
(vera_tileset_pixel 0 0 23 0 VERA_COLOR_GRAYSCALE_7)
(vera_tileset_pixel 0 0 24 0 VERA_COLOR_GRAYSCALE_8)
(vera_tileset_pixel 0 0 25 0 VERA_COLOR_GRAYSCALE_9)
(vera_tileset_pixel 0 0 26 0 VERA_COLOR_GRAYSCALE_10)
(vera_tileset_pixel 0 0 27 0 VERA_COLOR_GRAYSCALE_11)
(vera_tileset_pixel 0 0 28 0 VERA_COLOR_GRAYSCALE_12)
(vera_tileset_pixel 0 0 29 0 VERA_COLOR_GRAYSCALE_13)
(vera_tileset_pixel 0 0 30 0 VERA_COLOR_GRAYSCALE_14)
(vera_tileset_pixel 0 0 31 0 VERA_COLOR_GRAYSCALE_15)

(vera :irqline 0)
(vera :linecapture :enable)
(loop (if (= (vera :linecapture :enabled) 0) (return)))
(dotimes (ii 32)
  (print (vera :linecapture :pixel :x ii)))



'end
