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
(vera_sprite_init 2)
(vera_sprite_tile 2 0 2)
(vera_sprite_z_depth 2 VERA_SPRITE_Z_L1)
(dotimes (yy h)
  (dotimes (xx (min w (1+ yy)))
    (vera_sprite_pixel 1 xx yy VERA_COLOR_CYAN)))












(vera_sprite_x 1 40)
(vera_sprite_y 1 50)
(vera_sprite_x 2 200)
(vera_sprite_y 2 200)
(vera_sprite_col_mask 1 1)
(vera_sprite_col_mask 2 1)
(vera_sprite_col_mask 1)
(vera_sprite_col_mask 2)
(vera_ien VERA_IRQ_SPRCOL 1)




(vera_isr)

(vera_sprite_x 2 40)
(vera_sprite_y 2 50)




(vera_isr)
'end
