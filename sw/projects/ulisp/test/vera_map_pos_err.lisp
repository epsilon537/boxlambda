'start
(defvar cols 32)
(defvar rows 32)
(vera_map 0 cols rows VERA_MAP_TYPE_TXT16)
(vera_map_entry 0 cols 0
                (logior (ash VERA_COLOR_BLUE 12) (ash VERA_COLOR_WHITE 8) 1))
(vera_map_entry 0 0 rows
                (logior (ash VERA_COLOR_PURPLE 12) (ash VERA_COLOR_YELLOW 8) 1))
(vera_map_deinit 0)
(setq cols 64)
(setq rows 64)
(vera_map 0 cols rows VERA_MAP_TYPE_TXT16)
(vera_map_entry 0 cols 0
                (logior (ash VERA_COLOR_BLUE 12) (ash VERA_COLOR_WHITE 8) 1))
(vera_map_entry 0 0 rows
                (logior (ash VERA_COLOR_PURPLE 12) (ash VERA_COLOR_YELLOW 8) 1))
(vera_map_deinit 0)
(setq cols 128)
(setq rows 128)
(vera_map 0 cols rows VERA_MAP_TYPE_TXT16)
(vera_map_entry 0 cols 0
                (logior (ash VERA_COLOR_BLUE 12) (ash VERA_COLOR_WHITE 8) 1))
(vera_map_entry 0 0 rows
                (logior (ash VERA_COLOR_PURPLE 12) (ash VERA_COLOR_YELLOW 8) 1))
(vera_map_deinit 0)
(setq cols 256)
(setq rows 256)
(vera_map 0 cols rows VERA_MAP_TYPE_TXT16)
(vera_map_entry 0 cols 0
                (logior (ash VERA_COLOR_BLUE 12) (ash VERA_COLOR_WHITE 8) 1))
(vera_map_entry 0 0 rows
                (logior (ash VERA_COLOR_PURPLE 12) (ash VERA_COLOR_YELLOW 8) 1))
(vera_map_deinit 0)
'end
