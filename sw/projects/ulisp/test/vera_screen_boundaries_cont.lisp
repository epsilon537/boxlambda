'start
(vera_init)
(vera_display_enable 1)
(vera_bordercolor VERA_COLOR_BLUE)
(vera_screen_boundaries 100 540 100 380)
(vera_irqline 379)
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(vera_line_capture_read_pixel 99)
(vera_line_capture_read_pixel 100)
(vera_line_capture_read_pixel 539)
(vera_line_capture_read_pixel 540)
(vera_irqline 380)
(vera_line_capture_enable 1)
(loop (if (= (vera_line_capture_enable) 0) (return)))
(vera_line_capture_read_pixel 100)
(vera_line_capture_read_pixel 539)
(vera_display_enable 0)
(vera_display_enable)
'end


