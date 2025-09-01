
;const char LispLibrary[] =  R"lisplibrary(

(defvar VERA_IRQ_VSYNC 1)
(defvar VERA_IRQ_LINE 2)
(defvar VERA_IRQ_SPRCOL 4)

(defvar VERA_SCANLINE_VISIBLE_MAX 479)
(defvar VERA_SCANLINE_MAX 524)

(defvar VERA_HSTOP_MAX 1023)
(defvar VERA_VSTOP_MAX 1023)

(defvar VERA_MAX_NUM_TILES_IN_TILESET 1024)

(defvar VERA_NUM_LAYERS 2)

(defvar VERA_NUM_SPRITE_BANKS 2)
(defvar VERA_NUM_SPRITES_IN_BANK 64)
(defvar VERA_NUM_SPRITES (* VERA_NUM_SPRITE_BANKS VERA_NUM_SPRITES_IN_BANK))
(defvar VERA_MAX_SPRITE_ID 127)

(defvar VERA_NUM_MAPS 32)
(defvar VERA_NUM_TILESETS 32)

(defvar VERA_COLOR_BLACK 0)
(defvar VERA_COLOR_WHITE 1)
(defvar VERA_COLOR_RED 2)
(defvar VERA_COLOR_CYAN 3)
(defvar VERA_COLOR_PURPLE 4)
(defvar VERA_COLOR_GREEN 5)
(defvar VERA_COLOR_BLUE 6)
(defvar VERA_COLOR_YELLOW 7)
(defvar VERA_COLOR_ORANGE 8)
(defvar VERA_COLOR_BROWN 9)
(defvar VERA_COLOR_LIGHT_RED 10)
(defvar VERA_COLOR_DARK_GREY 11)
(defvar VERA_COLOR_GREY 12)
(defvar VERA_COLOR_LIGHT_GREEN 13)
(defvar VERA_COLOR_LIGHT_BLUE 14)
(defvar VERA_COLOR_LIGHT_GREY 15)

(defvar VERA_COLOR_GRAYSCALE_0 16)
(defvar VERA_COLOR_GRAYSCALE_1 17)
(defvar VERA_COLOR_GRAYSCALE_2 18)
(defvar VERA_COLOR_GRAYSCALE_3 19)
(defvar VERA_COLOR_GRAYSCALE_4 20)
(defvar VERA_COLOR_GRAYSCALE_5 21)
(defvar VERA_COLOR_GRAYSCALE_6 22)
(defvar VERA_COLOR_GRAYSCALE_7 23)
(defvar VERA_COLOR_GRAYSCALE_8 24)
(defvar VERA_COLOR_GRAYSCALE_9 25)
(defvar VERA_COLOR_GRAYSCALE_10 26)
(defvar VERA_COLOR_GRAYSCALE_11 27)
(defvar VERA_COLOR_GRAYSCALE_12 28)
(defvar VERA_COLOR_GRAYSCALE_13 29)
(defvar VERA_COLOR_GRAYSCALE_14 30)
(defvar VERA_COLOR_GRAYSCALE_15 31)

(defvar VERA_MAP_TYPE_TXT16 0)
(defvar VERA_MAP_TYPE_TXT256 1)
(defvar VERA_MAP_TYPE_TILE 2)

(defvar VERA_SPRITE_Z_DIS 0)
(defvar VERA_SPRITE_Z_BG_L0 1)
(defvar VERA_SPRITE_Z_L0_L1 2)
(defvar VERA_SPRITE_Z_L1 3)

(defvar VERA_MAPENTRY_HFLIP_MASK 1024)
(defvar VERA_MAPENTRY_VFLIP_MASK 2048)
(defvar VERA_MAPENTRY_PAL_OFFSET_SHIFT 12)

(defun _make_assoc_table (args)
  (defun _make_assoc_table_loop (table args)
    (if (>= (length args) 2)
      (let ((table (cons (cons (first args) (second args)) table))
            (args (cddr args)))
        (_make_assoc_table_loop table args))
      (lambda (k &optional return_nil_on_miss)
        (let ((entry (assoc k table)))
          (if entry
            (cdr entry)
            (if return_nil_on_miss
              nil
              (error "Missing argument: ~a. Table: ~s" k table)))))))
  (_make_assoc_table_loop '() args))

(defun _dispatch (table action args)
  (let ((disp_entry (assoc action table)))
    (if disp_entry
      (funcall (cdr disp_entry) args)
      (error "Dispatch keyword not found. Table: ~a keyword: ~a" table action))))

(defun _vera_init (args)
  (vera_init))

(defun _vera_map_res (map)
  (mapcan (lambda (k v) (list k v))
            '(:addr :width :height :map_type) map))

(defun _vera_map_init (table)
  (_vera_map_res
    (vera_map (table :id)
              (table :width)
              (table :height)
              (table :map_type))))

(defun _vera_map_deinit (table)
  (vera_map_deinit (table :id)))

(defun _vera_map_entry (table)
  (let ((col (table :x))
        (row (table :y))
        (val (table :val t)))
    (if val
      (vera_map_entry id col row val)
      (vera_map_entry id col row))))

(defun _vera_map_info (table)
  (_vera_map_res (vera_map (table :id))))

(defvar vera_map_dispatch '(
  (:init . _vera_map_init)
  (:deinit . _vera_map_deinit)
  (:entry . _vera_map_entry)
  (:info . _vera_map_info)))

(defun _vera_map (args)
  (if (>= (length args) 2)
    (let* ((id (first args))
           (action (second args))
           (args (cons :id (cons id (cddr args)))))
        (_dispatch vera_map_dispatch action (_make_assoc_table args)))
    (error "Map missing args")))

(defun _vera_tileset_res (tileset)
  (mapcan (lambda (k v) (list k v))
            '(:addr :width :height :bpp :num_tiles :tilesize_bytes) tileset))

(defun _vera_tileset_init (table)
  (_vera_tileset_res
    (vera_tileset (table :id)
                (table :width)
                (table :height)
                (table :bpp)
                (table :num_tiles))))

(defun _vera_tileset_deinit (table)
  (vera_tileset_deinit (table :id)))

(defun _vera_tileset_pixel (table)
    (let ((tile_idx (table :tile_idx))
          (x (table :x))
          (y (table :y))
          (val (table :val t)))
      (if val
        (vera_tileset_pixel id tile_idx x y val)
        (vera_tileset_pixel id tile_idx x y))))

(defun _vera_tileset_info (table)
  (_vera_tileset_res
    (vera_tileset (table :id))))

(defvar vera_tileset_dispatch '(
  (:init . _vera_tileset_init)
  (:deinit . _vera_tileset_deinit)
  (:pixel . _vera_tileset_pixel)
  (:info . _vera_tileset_info)))

(defun _vera_tileset (args)
  (if (>= (length args) 2)
    (let* ((id (first args))
           (action (second args))
           (args (cons :id (cons id (cddr args)))))
        (_dispatch vera_tileset_dispatch action (_make_assoc_table args)))
    (error "Tileset missing args")))

(defun _vera_linecapture_enable (table)
  (vera_line_capture_enable 1))

(defun _vera_linecapture_disable (table)
  (vera_line_capture_enable 0))

(defun _vera_linecapture_pixel_res (pixel)
  (mapcan (lambda (k v) (list k v))
            '(:r :g :b) pixel))

(defun _vera_linecapture_pixel (table)
  (_vera_linecapture_pixel_res
    (vera_line_capture_read_pixel (table :x))))

(defun _vera_linecapture_enabled (table)
  (vera_line_capture_enable))

(defvar vera_linecapture_dispatch '(
  (:enable . _vera_linecapture_enable)
  (:disable . _vera_linecapture_disable)
  (:pixel . _vera_linecapture_pixel)
  (:enabled . _vera_linecapture_enabled)))

(defun _vera_linecapture (args)
  (if args
    (let ((action (first args))
           (args  (cdr args)))
        (_dispatch vera_linecapture_dispatch action (_make_assoc_table args)))
    (error "Linecapture missing args")))

(defun _vera_spritebank_select (table)
  (vera_sprite_bank (table :select)))

(defun _vera_spritebank_selected (table)
  (vera_sprite_bank))

(defvar vera_spritebank_dispatch '(
  (:select . _vera_spritebank_select)
  (:selected . _vera_spritebank_selected)))

(defun _vera_spritebank (args)
  (if args
    (let ((action (first args)))
        (_dispatch vera_spritebank_dispatch action (_make_assoc_table args)))
    (error "Spritebank missing args")))

(defun _vera_ien_set (table)
  (vera_ien (table :mask) 1))

(defun _vera_ien_clr (table)
  (vera_ien (table :mask) 0))

(defun _vera_ien_get (table)
  (vera_ien))

(defvar vera_ien_dispatch '(
  (:set . _vera_ien_set)
  (:clr . _vera_ien_clr)
  (:get . _vera_ien_get)))

(defun _vera_ien (args)
  (if args
    (let ((action (first args)))
        (_dispatch vera_ien_dispatch action (_make_assoc_table (cdr args))))
    (error "IEN missing args")))

(defun _vera_isr_set (table)
  (vera_isr (table :mask)))

(defun _vera_isr_get (table)
  (vera_isr))

(defvar vera_isr_dispatch '(
  (:set . _vera_isr_set)
  (:get . _vera_isr_get)))

(defun _vera_isr (args)
  (if args
    (let ((action (first args)))
        (_dispatch vera_isr_dispatch action (_make_assoc_table (cdr args))))
    (error "ISR missing args")))

(defvar vera_irqline_dispatch '(
  (:set . _vera_irqline_set)
  (:info . _vera_irqline_get)))

(defun _vera_irqline (args)
  (if args
    (vera_irqline (first args))
    (vera_irqline)))

(defun _vera_scanline (args)
  (vera_scanline))

(defun _vera_display_enable (table)
  (vera_display_enable 1))

(defun _vera_display_disable (table)
  (vera_display_enable 0))

(defun _vera_display_enabled (table)
  (vera_display_enable))

(defvar vera_display_dispatch '(
  (:enable . _vera_display_enable)
  (:disable . _vera_display_disable)
  (:enabled . _vera_display_enabled)))

(defun _vera_display (args)
  (if args
    (let ((action (first args)))
        (_dispatch vera_display_dispatch action '()))
    (error "Display missing args")))

(defun _vera_sprites_enable (table)
  (vera_sprites_enable 1))

(defun _vera_sprites_disable (table)
  (vera_sprites_enable 0))

(defun _vera_sprites_enabled (table)
  (vera_sprites_enable))

(defvar vera_sprites_dispatch '(
  (:enable . _vera_sprites_enable)
  (:disable . _vera_sprites_disable)
  (:enabled . _vera_sprites_enabled)))

(defun _vera_sprites (args)
  (if args
    (let ((action (first args)))
        (_dispatch vera_sprites_dispatch action '()))
    (error "sprites missing args")))

(defun _vera_hscale (args)
  (if args
    (vera_hscale (first args))
    (vera_hscale)))

(defun _vera_vscale (args)
  (if args
    (vera_vscale (first args))
    (vera_vscale)))

(defun _vera_bordercolor (args)
  (if args
    (vera_bordercolor (first args))
    (vera_bordercolor)))

(defun _vera_screen_boundaries_res (boundaries)
  (mapcan (lambda (k v) (list k v))
            '(:hstart :hstop :vstart :vstop) boundaries))

(defun _vera_screen_boundaries (args)
  (_vera_screen_boundaries_res
    (if args
      (let ((table (_make_assoc_table args)))
        (vera_screen_boundaries
          (table :hstart) (table :hstop) (table :vstart) (table :vstop)))
      (vera_screen_boundaries))))

(defun _vera_layer_enable (args)
  (vera_layer_enable (first args) 1))

(defun _vera_layer_disable (args)
  (vera_layer_enable (first args) 0))

(defun _vera_layer_enabled (args)
  (vera_layer_enable (first args)))

(defun _vera_layer_hscroll (args)
  (let ((id (first args))
        (val (second args)))
    (if val
      (vera_layer_hscroll id val)
      (vera_layer_hscroll id))))

(defun _vera_layer_vscroll (args)
  (let ((id (first args))
        (val (second args)))
    (if val
      (vera_layer_vscroll id val)
      (vera_layer_vscroll id))))

(defun _vera_layer_map (args)
  (let ((id (first args))
        (val (second args)))
    (if val
      (vera_layer_map id val)
      (vera_layer_map id))))

(defun _vera_layer_tileset (args)
  (let ((id (first args))
        (val (second args)))
    (if val
      (vera_layer_tileset id val)
      (vera_layer_tileset id))))

(defun _vera_layer_bitmap_res (bitmap)
  (mapcan (lambda (k v) (list k v))
            '(:tileset :tile_idx) bitmap))

(defun _vera_layer_bitmap (args)
  (let ((id (first args))
        (args (cdr args)))
    (_vera_layer_bitmap_res
      (if args
        (let ((table (_make_assoc_table args)))
          (vera_layer_bitmap id (table :tileset) (table :tile_idx)))
        (vera_layer_bitmap id)))))

(defun _vera_layer_pal_offset (args)
  (let ((id (first args))
        (val (second args)))
    (if val
      (vera_layer_pal_offset id val)
      (vera_layer_pal_offset id))))

(defvar vera_layer_dispatch '(
  (:enable . _vera_layer_enable)
  (:disable . _vera_layer_disable)
  (:enabled . _vera_layer_enabled)
  (:hscroll . _vera_layer_hscroll)
  (:vscroll . _vera_layer_vscroll)
  (:map . _vera_layer_map)
  (:tileset . _vera_layer_tileset)
  (:bitmap . _vera_layer_bitmap)
  (:pal_offset . _vera_layer_pal_offset)))

(defun _vera_layer (args)
  (if (>= (length args) 2)
    (let* ((id (first args))
           (action (second args))
           (args (cons id (cddr args))))
      (_dispatch vera_layer_dispatch action args))
    (error "Layer missing args")))

(defun _vera_palette_restore (args)
  (vera_palette))

(defun _vera_palette_res (palette)
  (mapcan (lambda (k v) (list k v))
            '(:r :g :b) palette))

(defun _vera_palette_idx (args)
  (if args
    (let ((idx (first args))
          (args (cdr args)))
      (_vera_palette_res
        (if args
          (let ((table (_make_assoc_table args)))
            (vera_palette idx (table :r) (table :g) (table :b)))
          (vera_palette idx))))
    (error "Palette missing args")))

(defvar vera_palette_dispatch '(
  (:restore . _vera_palette_restore)
  (:idx     . _vera_palette_idx)))

(defun _vera_palette (args)
  (if args
    (let ((action (first args))
          (args (cdr args)))
      (_dispatch vera_palette_dispatch action args))
    (error "Layer missing args")))

(defun _vera_sprite_init (args)
  (vera_sprite_init (first args)))

(defun _vera_sprite_tileset_res (tileset)
  (mapcan (lambda (k v) (list k v))
            '(:tileset :tile_idx) tileset))

(defun _vera_sprite_tileset (args)
  (let ((id (first args))
        (args (rest args)))
    (_vera_sprite_tileset_res
      (if args
        (let ((tileset (first args))
              (table (_make_assoc_table (rest args))))
          (vera_sprite_tile id tileset (table :tile_idx)))
        (vera_sprite_tile id)))))

(defun _vera_sprite_x (args)
  (let ((id (first args))
        (args (rest args)))
    (if args
      (let ((x (first args))
            (table (_make_assoc_table (rest args))))
        (vera_sprite_x id x)
        (let ((y (table :y t)))
          (if y
            (vera_sprite_y id y))))
      (vera_sprite_x id))))

(defun _vera_sprite_y (args)
  (let ((id (first args))
        (args (rest args)))
    (if args
      (let ((y (first args)))
        (vera_sprite_y id y))
      (vera_sprite_y id))))

(defun _vera_sprite_z (args)
  (let ((id (first args))
        (args (rest args)))
    (if args
      (let ((z (first args)))
        (vera_sprite_z_depth id z))
      (vera_sprite_z_depth id))))

(defun _vera_sprite_pal_offset (args)
  (let ((id (first args))
        (args (rest args)))
    (if args
      (let ((offset (first args)))
        (vera_sprite_pal_offset id offset))
      (vera_sprite_pal_offset id))))

(defun _vera_sprite_mask (args)
  (let ((id (first args))
        (args (rest args)))
    (if args
      (let ((mask (first args)))
        (vera_sprite_col_mask id mask))
      (vera_sprite_col_mask id))))

(defun _vera_sprite_hflip_enable (id)
  (vera_sprite_hflip id 1))

(defun _vera_sprite_hflip_disable (id)
  (vera_sprite_hflip id 0))

(defun _vera_sprite_hflip_enabled (id)
  (vera_sprite_hflip id))

(defvar _vera_sprite_hflip_dispatch '(
  (:enable . _vera_sprite_hflip_enable)
  (:disable . _vera_sprite_hflip_disable)
  (:enabled . _vera_sprite_hflip_enabled)))

(defun _vera_sprite_hflip (args)
  (let ((id (first args))
        (action (second args)))
    (if action
      (_dispatch _vera_sprite_hflip_dispatch action id)
      (error "Hflip missing args."))))

(defun _vera_sprite_vflip_enable (id)
  (vera_sprite_vflip id 1))

(defun _vera_sprite_vflip_disable (id)
  (vera_sprite_vflip id 0))

(defun _vera_sprite_vflip_enabled (id)
  (vera_sprite_vflip id))

(defvar _vera_sprite_vflip_dispatch '(
  (:enable . _vera_sprite_vflip_enable)
  (:disable . _vera_sprite_vflip_disable)
  (:enabled . _vera_sprite_vflip_enabled)))

(defun _vera_sprite_vflip (args)
  (let ((id (first args))
        (action (second args)))
    (if action
      (_dispatch _vera_sprite_vflip_dispatch action id)
      (error "vflip missing args."))))

(defvar _vera_sprite_dispatch '(
  (:init . _vera_sprite_init)
  (:tileset . _vera_sprite_tileset)
  (:x . _vera_sprite_x)
  (:y . _vera_sprite_y)
  (:z . _vera_sprite_z)
  (:pal_offset . _vera_sprite_pal_offset)
  (:mask . _vera_sprite_mask)
  (:hflip . _vera_sprite_hflip)
  (:vflip . _vera_sprite_vflip)))

(defun _vera_sprite (args)
  (if (>= (length args) 2)
    (let* ((id (first args))
           (action (second args))
           (args (cons id (cddr args))))
      (_dispatch _vera_sprite_dispatch action args))
    (error "Sprite missing args.")))

(defun _vera_help (args)
  (format t "(vera :init)~%")
  (format t "(vera :map <id> :init :width <w> :height <h> :map_type <t>)~%")
  (format t "(vera :map <id> :deinit)~%")
  (format t "(vera :map <id> :entry :x <x> :y <y> [:val <v>])~%")
  (format t "(vera :map <id> :info)~%")
  (format t "(vera :tileset <id> :init :width <w> :height <h> :bpp <b> :num_tiles <n>)~%")
  (format t "(vera :tileset <id> :deinit)~%")
  (format t "(vera :tileset <id> :pixel :tile_idx <idx> :x <x> :y <y> [:val <v>])~%")
  (format t "(vera :tileset <id> :info)~%")
  (format t "(vera :linecapture :enable)~%")
  (format t "(vera :linecapture :disable)~%")
  (format t "(vera :linecapture :enabled)~%")
  (format t "(vera :linecapture :pixel :x <x>)~%")
  (format t "(vera :spritebank :select <idx>)~%")
  (format t "(vera :spritebank :selected)~%")
  (format t "(vera :ien :set :mask <m>)~%")
  (format t "(vera :ien :clr :mask <m>)~%")
  (format t "(vera :ien :get)~%")
  (format t "(vera :isr :set :mask <m>)~%")
  (format t "(vera :isr :get)~%")
  (format t "(vera :irqline [<l>])~%")
  (format t "(vera :scanline)~%")
  (format t "(vera :display :enable)~%")
  (format t "(vera :display :disable)~%")
  (format t "(vera :display :enabled)~%")
  (format t "(vera :sprites :enable)~%")
  (format t "(vera :sprites :disable)~%")
  (format t "(vera :sprites :enabled)~%")
  (format t "(vera :hscale [<h>])~%")
  (format t "(vera :vscale [<v>])~%")
  (format t "(vera :bordercolor [<c>])~%")
  (format t "(vera :boundaries [:hstart <hs> :hstop <he> :vstart <vs> :vstop <ve>])~%")
  (format t "(vera :layer <id> :enable)~%")
  (format t "(vera :layer <id> :disable)~%")
  (format t "(vera :layer <id> :enabled)~%")
  (format t "(vera :layer <id> :hscroll [<h>])~%")
  (format t "(vera :layer <id> :vscroll [<v>])~%")
  (format t "(vera :layer <id> :map [<m>])~%")
  (format t "(vera :layer <id> :tileset [<t>])~%")
  (format t "(vera :layer <id> :bitmap [:tileset <t> :tile_idx <i>])~%")
  (format t "(vera :layer <id> :pal_offset [<o>])~%")
  (format t "(vera :palette :restore)~%")
  (format t "(vera :palette :idx <i> [:r <r> :g <g> :b <b>])~%")
  (format t "(vera :sprite <id> :init)~%")
  (format t "(vera :sprite <id> :tileset <t> :tile_idx <i>)~%")
  (format t "(vera :sprite <id> :tileset)~%")
  (format t "(vera :sprite <id> :x [<x>])~%")
  (format t "(vera :sprite <id> :y [<y>])~%")
  (format t "(vera :sprite <id> :x <x> :y <y>)~%")
  (format t "(vera :sprite <id> :z [<z>])~%")
  (format t "(vera :sprite <id> :pal_offset [<o>])~%")
  (format t "(vera :sprite <id> :mask [<m>])~%")
  (format t "(vera :sprite <id> :hflip :enable)~%")
  (format t "(vera :sprite <id> :hflip :disable)~%")
  (format t "(vera :sprite <id> :hflip :enabled)~%")
  (format t "(vera :sprite <id> :vflip :enable)~%")
  (format t "(vera :sprite <id> :vflip :disable)~%")
  (format t "(vera :sprite <id> :vflip :enabled)~%")
  )

(defvar vera_dispatch '(
  (:init . _vera_init)
  (:map . _vera_map)
  (:tileset . _vera_tileset)
  (:linecapture . _vera_linecapture)
  (:spritebank . _vera_spritebank)
  (:ien . _vera_ien)
  (:isr . _vera_isr)
  (:irqline . _vera_irqline)
  (:scanline . _vera_scanline)
  (:display . _vera_display)
  (:sprites . _vera_sprites)
  (:hscale . _vera_hscale)
  (:vscale . _vera_vscale)
  (:bordercolor . _vera_bordercolor)
  (:boundaries . _vera_screen_boundaries)
  (:layer . _vera_layer)
  (:palette . _vera_palette)
  (:sprite . _vera_sprite)
  (:help . _vera_help)))

(defun vera (&rest args)
  (if args
    (_dispatch vera_dispatch (first args) (rest args))
    (_vera_help '())))

)lisplibrary";

