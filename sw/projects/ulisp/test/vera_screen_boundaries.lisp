'start
(vera_init)
(vera_display_enable 1)
(vera_display_enable)
(vera_bordercolor VERA_COLOR_BLUE)
(vera_bordercolor)
(vera_screen_boundaries 100 540 100 380)
(vera_screen_boundaries)
(vera_irqline 99)
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(vera_line_capture_read_pixel 100)
(vera_line_capture_read_pixel 539)
(vera_irqline 100)
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(vera_line_capture_read_pixel 99)
(vera_line_capture_read_pixel 100)
(vera_line_capture_read_pixel 539)
(vera_line_capture_read_pixel 540)
'end


