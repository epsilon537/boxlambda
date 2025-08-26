'start
(vera_display_enable 1)
(vera_sprites_enable 1)
(defvar w 64)
(defvar h 64)
(defvar bpp 8)
(vera_tileset 0 w h bpp 4)
(vera_tileset 0)
(vera_sprite_init 1)
(vera_sprite_tile 1 0 2)
(vera_sprite_z_depth 1 VERA_SPRITE_Z_L1)
(dotimes (yy h)
  (dotimes (xx (min w (1+ yy)))
    (vera_tileset_pixel 0 2 xx yy VERA_COLOR_CYAN)))


















(vera_irqline 0)
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(print (vera_line_capture_read_pixel 0))
(vera_sprite_pal_offset 1 1)
(vera_sprite_pal_offset 1)
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(print (vera_line_capture_read_pixel 0))
'end
