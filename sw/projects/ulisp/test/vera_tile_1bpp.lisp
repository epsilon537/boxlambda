'start
(vera_display_enable 1)
(vera_layer_enable 0 1)
(vera_map 0 32 32 VERA_MAP_TYPE_TXT16)
(vera_layer_map 0 0)
(vera_map_entry 0 0 0 257)
(dolist (w (list 8 16))
  (dolist (h (list 8 16))
    (vera_tileset 0 w h 1 8)
    (vera_layer_tileset 0 0)
    (vera_tileset_pixel 0 1 (- w 1) (- h 1) 1)
    (print (vera_tileset_pixel 0 1 (- w 1) (- h 1)))
    (vera_irqline (- h 1))
    (vera_line_capture_enable 1)
    (loop (if (= (vera_line_capture_enable) 0) (return)))
    (vera_line_capture_enable 1)
    (loop (if (= (vera_line_capture_enable) 0) (return)))
    (print (vera_line_capture_read_pixel (- w 1)))
    (vera_tileset_pixel 0 1 (- w 1) (- h 1) 0)
    (vera_tileset_deinit 0)))






'end
