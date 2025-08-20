'start
(vera_display_enable 1)
(vera_layer_enable 0 1)
(dolist (w (list 320 640))
  (dolist (h (list 32 64))
    (vera_tileset 0 w h 2 1)
    (print (vera_tileset 0))
    (vera_layer_bitmap 0 0 0)
    (vera_tileset_pixel 0 0 0 0 101)
    (vera_tileset_pixel 0 0 (- w 1) 0 101)
    (vera_tileset_pixel 0 0 0 (- h 1) 101)
    (vera_tileset_pixel 0 0 (- w 1) (- h 1) 101)
    (print (vera_tileset_pixel 0 0 0 0))
    (print (vera_tileset_pixel 0 0 (- w 1) 0))
    (print (vera_tileset_pixel 0 0 0 (- h 1)))
    (print (vera_tileset_pixel 0 0 (- w 1) (- h 1)))
    (vera_irqline (- h 1))
    (vera_line_capture_enable 1)
    (loop (if (= (vera_line_capture_enable) 0) (return)))
    (vera_line_capture_enable 1)
    (loop (if (= (vera_line_capture_enable) 0) (return)))
    (print (vera_line_capture_read_pixel 0))
    (print (vera_line_capture_read_pixel 1))
    (print (vera_line_capture_read_pixel (- w 2)))
    (print (vera_line_capture_read_pixel (- w 1)))
    (vera_irqline (- h 1))
    (vera_line_capture_enable 1)
    (loop (if (= (vera_line_capture_enable) 0) (return)))
    (print (vera_line_capture_read_pixel 0))
    (print (vera_line_capture_read_pixel 1))
    (print (vera_line_capture_read_pixel (- w 2)))
    (print (vera_line_capture_read_pixel (- w 1)))
    (vera_tileset_pixel 0 0 0 0 0)
    (vera_tileset_pixel 0 0 (- w 1) 0 0)
    (vera_tileset_pixel 0 0 0 (- h 1) 0)
    (vera_tileset_pixel 0 0 (- w 1) (- h 1) 0)
    (vera_tileset_deinit 0)))






'end
