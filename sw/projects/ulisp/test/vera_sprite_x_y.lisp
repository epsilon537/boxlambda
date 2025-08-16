'start
(vera_display_enable 1)
(vera_sprite_enable 1)
(defvar w 64)
(defvar h 64)
(defvar bpp 8)
(vera_tileset 0 w h bpp 8)
(vera_tileset 0)
(vera_sprite_init 1)
(vera_sprite_tile 1 0 2)
(vera_sprite_z_depth 1 VERA_SPRITE_Z_L1)
(dotimes (yy h)
  (dotimes (xx (min w (1+ yy)))
    (vera_sprite_pixel 1 xx yy VERA_COLOR_CYAN)))


















(vera_sprite_x 1 40)
(vera_sprite_y 1 50)
(vera_sprite_x 1)
(vera_sprite_y 1)
(vera_irqline 50)
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(print (vera_line_capture_read_pixel 40))
(print (vera_line_capture_read_pixel 41))
(vera_irqline (+ 50 (- h 1)))
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(print (vera_line_capture_read_pixel 40))
(print (vera_line_capture_read_pixel 47))
(print (vera_line_capture_read_pixel (+ 40 63)))
(print (vera_line_capture_read_pixel (+ 40 64)))
(vera_irqline (+ 50 h))
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(print (vera_line_capture_read_pixel 40))
(print (vera_line_capture_read_pixel 47))

'end
