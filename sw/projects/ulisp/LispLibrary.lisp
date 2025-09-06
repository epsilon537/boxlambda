
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

(defun _make_assoc_table_lookup (table)
  (lambda (k &optional return_nil_on_miss)
    (let ((entry (assoc k table)))
      (if entry
        (cdr entry)
        (if return_nil_on_miss
          nil
          (error "Unknown argument: ~a. Table: ~s" k table))))))

(defun _make_assoc_table (args)
  (defun _make_assoc_table_loop (table args)
    (if args
      (let ((key (first args)) (val (second args)))
        (if (numberp key) (error "Malformed argument list: ~a." args))
        (if (numberp val)
          (let ((table (cons (cons key val) table)) (args (cddr args)))
            (_make_assoc_table_loop table args))
          (let ((table (cons (cons key nil) table)) (args (cdr args)))
            (_make_assoc_table_loop table args))))
      (reverse table)))
  (_make_assoc_table_loop '() args))

(defun _last (l) (nth (1- (length l)) l))

(defun _dispatch (disp_table arg_lookup iter)
  (if iter
    (let* ((action (first (first iter)))
           (disp_tbl_entry (assoc action disp_table))
           (iter (rest iter)))
      (if disp_tbl_entry
        (funcall (cdr disp_tbl_entry) arg_lookup iter)
        (error "Dispatch keyword not found. Table: ~a keyword: ~a" disp_table action)))
    (funcall (cdr (_last disp_table)) arg_lookup iter)))

(defun _vera_map_res (map)
  (mapcan (lambda (k v) (list k v)) '(:addr :width :height :map_type) map))

(defun _vera_map_init (arg_lookup iter)
  (_vera_map_res
    (vera_map (arg_lookup :map) (arg_lookup :width) (arg_lookup :height) (arg_lookup :map_type))))

(defun _vera_map_entry (arg_lookup iter)
  (let ((id (arg_lookup :map)) (col (arg_lookup :x)) (row (arg_lookup :y)) (val (arg_lookup :val t)))
    (if val
      (vera_map_entry id col row val)
      (vera_map_entry id col row))))

(defvar _vera_map_dispatch '(
  (:width . _vera_map_init)
  (:height . _vera_map_init)
  (:map_type . _vera_map_init)
  (:deinit . (lambda (arg_lookup iter) (vera_map_deinit (arg_lookup :map))))
  (:entry . _vera_map_entry)
  (:info . (lambda (arg_lookup iter) (_vera_map_res (vera_map (arg_lookup :map)))))))

(defun _vera_map (arg_lookup iter)
  (_dispatch _vera_map_dispatch arg_lookup iter))

(defun _vera_tileset_res (tileset)
  (mapcan (lambda (k v) (list k v))
            '(:addr :width :height :bpp :num_tiles :tilesize_bytes) tileset))

(defun _vera_tileset_init (arg_lookup iter)
  (_vera_tileset_res
    (vera_tileset (arg_lookup :tileset) (arg_lookup :width) (arg_lookup :height)
                (arg_lookup :bpp) (arg_lookup :num_tiles))))

(defvar _vera_tileset_dispatch '(
  (:width . _vera_tileset_init)
  (:height . _vera_tileset_init)
  (:bpp . _vera_tileset_init)
  (:num_tiles . _vera_tileset_init)
  (:deinit . (lambda (arg_lookup iter) (vera_tileset_deinit (arg_lookup :tileset))))
  (:info . (lambda (arg_lookup iter) (_vera_tileset_res (vera_tileset (arg_lookup :tileset)))))))

(defun _vera_tileset (arg_lookup iter)
  (_dispatch _vera_tileset_dispatch arg_lookup iter))

(defun _vera_pixel (arg_lookup iter)
    (let ((tileset (arg_lookup :tileset)) (tile (arg_lookup :tile))
          (x (arg_lookup :x)) (y (arg_lookup :y)) (val (arg_lookup :val t)))
      (if val
        (vera_tileset_pixel tileset tile x y val)
        (vera_tileset_pixel tileset tile x y))))

(defun _vera_linecapture_pixel_res (pixel)
  (mapcan (lambda (k v) (list k v)) '(:r :g :b) pixel))

(defun _vera_linecapture_pixel (arg_lookup iter)
  (_vera_linecapture_pixel_res (vera_line_capture_read_pixel (arg_lookup :x))))

(defvar _vera_linecapture_dispatch '(
  (:enable . (lambda (arg_lookup iter) (vera_line_capture_enable 1)))
  (:disable . (lambda (arg_lookup iter) (vera_line_capture_enable 0)))
  (:pixel . _vera_linecapture_pixel)
  (:enabled . (lambda (arg_lookup iter) (vera_line_capture_enable)))))

(defun _vera_linecapture (arg_lookup iter)
  (_dispatch _vera_linecapture_dispatch arg_lookup iter))

(defvar _vera_spritebank_dispatch '(
  (:select . (lambda (arg_lookup iter) (vera_sprite_bank (arg_lookup :select))))
  (:selected . (lambda (arg_lookup iter) (vera_sprite_bank)))))

(defun _vera_spritebank (arg_lookup iter)
  (_dispatch _vera_spritebank_dispatch arg_lookup iter))

(defvar _vera_ien_dispatch '(
  (:set . (lambda (arg_lookup iter) (vera_ien (arg_lookup :mask) 1)))
  (:clr . (lambda (arg_lookup iter) (vera_ien (arg_lookup :mask) 0)))
  (:get . (lambda (arg_lookup iter) (vera_ien)))))

(defun _vera_ien (arg_lookup iter)
  (_dispatch _vera_ien_dispatch arg_lookup iter))

(defvar _vera_isr_dispatch '(
  (:set . (lambda (arg_lookup iter) (vera_isr (arg_lookup :mask))))
  (:get . (lambda (arg_lookup iter) (vera_isr)))))

(defun _vera_isr (arg_lookup iter)
  (_dispatch _vera_isr_dispatch arg_lookup iter))

(defun _vera_irqline (arg_lookup iter)
  (let ((irqline (arg_lookup :irqline)))
    (if irqline
      (vera_irqline irqline)
      (vera_irqline))))

(defvar _vera_display_dispatch '(
  (:enable . (lambda (arg_lookup iter) (vera_display_enable 1)))
  (:disable . (lambda (arg_lookup iter) (vera_display_enable 0)))
  (:enabled . (lambda (arg_lookup iter) (vera_display_enable)))))

(defun _vera_display (arg_lookup iter)
  (_dispatch _vera_display_dispatch arg_lookup iter))

(defvar _vera_sprites_dispatch '(
  (:enable . (lambda (arg_lookup iter) (vera_sprites_enable 1)))
  (:disable . (lambda (arg_lookup iter) (vera_sprites_enable 0)))
  (:enabled . (lambda (arg_lookup iter) (vera_sprites_enable)))))

(defun _vera_sprites (arg_lookup iter)
  (_dispatch _vera_sprites_dispatch arg_lookup iter))

(defun _vera_hscale (arg_lookup iter)
  (let ((hscale (arg_lookup :hscale)))
    (if hscale
      (progn
        (vera_hscale hscale)
        (if iter (_vera arg_lookup iter)))
      (vera_hscale))))

(defun _vera_vscale (arg_lookup iter)
  (let ((vscale (arg_lookup :vscale)))
    (if vscale
      (progn
        (vera_vscale vscale)
        (if iter (_vera arg_lookup iter)))
      (vera_vscale))))

(defun _vera_bordercolor (arg_lookup iter)
  (let ((bordercolor (arg_lookup :bordercolor)))
    (if bordercolor
      (vera_bordercolor bordercolor)
      (vera_bordercolor))))

(defun _vera_screen_boundaries_res (boundaries)
  (mapcan (lambda (k v) (list k v))
            '(:hstart :hstop :vstart :vstop) boundaries))

(defun _vera_screen_boundaries (arg_lookup iter)
  (_vera_screen_boundaries_res
    (if (consp iter)
      (vera_screen_boundaries (arg_lookup :hstart) (arg_lookup :hstop) (arg_lookup :vstart) (arg_lookup :vstop))
      (vera_screen_boundaries))))

(defun _vera_layer_hscroll (arg_lookup iter)
  (let ((layer (arg_lookup :layer)) (hscroll (arg_lookup :hscroll)))
    (if hscroll
      (progn
        (vera_layer_hscroll layer hscroll)
        (if iter (_vera_layer arg_lookup iter)))
      (vera_layer_hscroll layer))))

(defun _vera_layer_vscroll (arg_lookup iter)
  (let ((layer (arg_lookup :layer)) (vscroll (arg_lookup :vscroll t)))
    (if vscroll
      (progn
        (vera_layer_vscroll layer vscroll)
        (if iter (_vera_layer arg_lookup iter)))
      (vera_layer_vscroll layer))))

(defun _vera_layer_map (arg_lookup iter)
  (let ((layer (arg_lookup :layer)) (map (arg_lookup :map)))
    (if map
      (progn
        (vera_layer_map layer map)
        (if iter
            (_vera_layer arg_lookup iter)))
      (vera_layer_map layer))))

(defun _vera_layer_tileset (arg_lookup iter)
  (let ((layer (arg_lookup :layer))
        (tileset (arg_lookup :tileset t)))
    (if tileset
      (vera_layer_tileset layer tileset))
    (vera_layer_tileset layer)))

(defun _vera_layer_bitmap_res (bitmap)
  (mapcan (lambda (k v) (list k v)) '(:tileset :tile) bitmap))

(defun _vera_layer_bitmap (arg_lookup iter)
  (_vera_layer_bitmap_res
    (let ((layer (arg_lookup :layer)) (tileset (arg_lookup :tileset t)) (tile (arg_lookup :tile t)))
      (if (and tileset tile)
        (vera_layer_bitmap layer tileset tile)
        (vera_layer_bitmap layer)))))

(defun _vera_layer_pal_offset (arg_lookup iter)
  (let ((layer (arg_lookup :layer)) (pal_offset (arg_lookup :pal_offset t)))
    (if pal_offset
      (vera_layer_pal_offset layer pal_offset))
    (vera_layer_pal_offset layer)))

(defvar _vera_layer_dispatch '(
  (:enable . (lambda (arg_lookup iter) (vera_layer_enable (arg_lookup :layer) 1)))
  (:disable . (lambda (arg_lookup iter) (vera_layer_enable (arg_lookup :layer) 0)))
  (:enabled . (lambda (arg_lookup iter) (vera_layer_enable (arg_lookup :layer))))
  (:hscroll . _vera_layer_hscroll)
  (:vscroll . _vera_layer_vscroll)
  (:map . _vera_layer_map)
  (:tileset . _vera_layer_tileset)
  (:bitmap . _vera_layer_bitmap)
  (:pal_offset . _vera_layer_pal_offset)))

(defun _vera_layer (arg_lookup iter)
  (_dispatch _vera_layer_dispatch arg_lookup iter))

(defun _vera_palette_res (palette)
  (mapcan (lambda (k v) (list k v)) '(:r :g :b) palette))

(defun _vera_palette (arg_lookup iter)
  (let ((idx (arg_lookup :palette t)))
    (if idx
      (_vera_palette_res
        (if (consp iter)
          (vera_palette idx (arg_lookup :r) (arg_lookup :g) (arg_lookup :b))
          (vera_palette idx)))
      (progn
        (arg_lookup :restore)
        (vera_palette)))))

(defun _vera_sprite_tileset_res (tileset)
  (mapcan (lambda (k v) (list k v)) '(:tileset :tile) tileset))

(defun _vera_sprite_tileset (arg_lookup iter)
  (_vera_sprite_tileset_res
    (let ((id (arg_lookup :sprite)) (tileset (arg_lookup :tileset t)) (tile (arg_lookup :tile t)))
      (if (and tileset tile)
        (progn
          (vera_sprite_tile id tileset tile)
          (if (cdr iter) (_vera_sprite arg_lookup (cdr iter))))
        (vera_sprite_tile id)))))

(defun _vera_sprite_x (arg_lookup iter)
  (let ((id (arg_lookup :sprite)) (x (arg_lookup :x)))
    (if x
      (progn
        (vera_sprite_x id x)
        (if iter (_vera_sprite arg_lookup iter)))
      (vera_sprite_x id))))

(defun _vera_sprite_y (arg_lookup iter)
  (let ((id (arg_lookup :sprite)) (y (arg_lookup :y t)))
    (if y
      (progn
        (vera_sprite_y id y)
        (if iter (_vera_sprite arg_lookup iter)))
      (vera_sprite_y id))))

(defun _vera_sprite_z (arg_lookup iter)
  (let ((id (arg_lookup :sprite)) (z (arg_lookup :z t)))
    (if z
      (progn
        (vera_sprite_z_depth id z)
        (if iter (_vera_sprite arg_lookup iter)))
      (vera_sprite_z_depth id))))

(defun _vera_sprite_pal_offset (arg_lookup iter)
  (let ((id (arg_lookup :sprite)) (pal_offset (arg_lookup :pal_offset t)))
    (if pal_offset
      (progn
        (vera_sprite_pal_offset id pal_offset)
        (if iter (_vera_sprite arg_lookup iter)))
      (vera_sprite_pal_offset id))))

(defun _vera_sprite_mask (arg_lookup iter)
  (let ((id (arg_lookup :sprite)) (mask (arg_lookup :mask t)))
    (if mask
      (progn
        (vera_sprite_col_mask id mask)
        (if iter (_vera_sprite arg_lookup iter)))
      (vera_sprite_col_mask id))))

(defun _vera_sprite_hflip_enable (arg_lookup iter)
  (vera_sprite_hflip (arg_lookup :sprite) 1)
  (if iter (_vera_sprite arg_lookup iter)))

(defun _vera_sprite_hflip_disable (arg_lookup iter)
  (vera_sprite_hflip (arg_lookup :sprite) 0)
  (if iter (_vera_sprite arg_lookup iter)))

(defvar _vera_sprite_hflip_dispatch '(
  (:enable . _vera_sprite_hflip_enable)
  (:disable . _vera_sprite_hflip_disable)
  (:enabled . (lambda (arg_lookup iter) (vera_sprite_hflip (arg_lookup :sprite))))))

(defun _vera_sprite_hflip (arg_lookup iter)
  (_dispatch _vera_sprite_hflip_dispatch arg_lookup iter))

(defun _vera_sprite_vflip_enable (arg_lookup iter)
  (vera_sprite_vflip (arg_lookup :sprite) 1)
  (if iter (_vera_sprite arg_lookup iter)))

(defun _vera_sprite_vflip_disable (arg_lookup iter)
  (vera_sprite_vflip (arg_lookup :sprite) 0)
  (if iter (_vera_sprite arg_lookup iter)))

(defvar _vera_sprite_vflip_dispatch '(
  (:enable . _vera_sprite_vflip_enable)
  (:disable . _vera_sprite_vflip_disable)
  (:enabled . (lambda (arg_lookup iter) (vera_sprite_vflip (arg_lookup :sprite))))))

(defun _vera_sprite_vflip (arg_lookup iter)
  (_dispatch _vera_sprite_vflip_dispatch arg_lookup iter))

(defun _vera_sprite_info (arg_lookup iter)
  (let* ((id (arg_lookup :sprite))
         (tileset_info (vera_sprite_tile id))
         (tileset (if tileset_info (first tileset_info) '()))
         (tile (if tileset_info (second tileset_info) '()))
         (x (vera_sprite_x id))
         (y (vera_sprite_y id))
         (z (vera_sprite_z_depth id))
         (pal_offset (vera_sprite_pal_offset id))
         (mask (vera_sprite_col_mask id))
         (hflip (if (= 1 (vera_sprite_hflip id)) :enabled :disabled))
         (vflip (if (= 1 (vera_sprite_vflip id)) :enabled :disabled)))
    (list :tileset tileset :tile tile :x x :y y :z z :pal_offset pal_offset :mask mask :hflip hflip :vflip vflip)))

(defvar _vera_sprite_dispatch '(
  (:init . (lambda (arg_lookup iter) (vera_sprite_init (arg_lookup :sprite))))
  (:tileset . _vera_sprite_tileset)
  (:x . _vera_sprite_x)
  (:y . _vera_sprite_y)
  (:z . _vera_sprite_z)
  (:pal_offset . _vera_sprite_pal_offset)
  (:mask . _vera_sprite_mask)
  (:hflip . _vera_sprite_hflip)
  (:vflip . _vera_sprite_vflip)
  (:info . _vera_sprite_info)))

(defun _vera_sprite (arg_lookup iter)
  (_dispatch _vera_sprite_dispatch arg_lookup iter))

(defun _vera_help (args)
  (format t "(vera :init)~%")
  (format t "(vera :map <id> :width <w> :height <h> :map_type <t>)~%")
  (format t "(vera :map <id> :deinit)~%")
  (format t "(vera :map <id> :entry :x <x> :y <y> [:val <v>])~%")
  (format t "(vera :map <id> [:info])~%")
  (format t "(vera :tileset <id> :width <w> :height <h> :bpp <b> :num_tiles <n>)~%")
  (format t "(vera :tileset <id> :deinit)~%")
  (format t "(vera :tileset <id> [:info])~%")
  (format t "(vera :pixel :tileset <id> :tile <idx> :x <x> :y <y> [:val <v>])~%")
  (format t "(vera :linecapture :enable|:disable|:enabled)~%")
  (format t "(vera :linecapture :pixel :x <x>)~%")
  (format t "(vera :spritebank :select <idx>)~%")
  (format t "(vera :spritebank :selected)~%")
  (format t "(vera :ien :set|:clr :mask <m>)~%")
  (format t "(vera :ien :get)~%")
  (format t "(vera :isr :set :mask <m>)~%")
  (format t "(vera :isr :get)~%")
  (format t "(vera :irqline [<l>])~%")
  (format t "(vera :scanline)~%")
  (format t "(vera :display :enable|:disable|:enabled)~%")
  (format t "(vera :sprites :enable|:disable|:enabled)~%")
  (format t "(vera [:hscale <h>] [:vscale <v>])~%")
  (format t "(vera :hscale|:vscale)~%")
  (format t "(vera :bordercolor [<c>])~%")
  (format t "(vera :boundaries [:hstart <hs> :hstop <he> :vstart <vs> :vstop <ve>])~%")
  (format t "(vera :layer <id> :enable|:disable|:enabled)~%")
  (format t "(vera :layer <id> [:hscroll <h>] [:vscroll <v>])~%")
  (format t "(vera :layer <id> :hscroll|:vscroll)~%")
  (format t "(vera :layer <id> :map [<m>] [:tileset t])~%")
  (format t "(vera :layer <id> :tileset [<t>] [:map <m>])~%")
  (format t "(vera :layer <id> :bitmap [:tileset <t> :tile <i>])~%")
  (format t "(vera :layer <id> :pal_offset [<o>])~%")
  (format t "(vera :palette :restore)~%")
  (format t "(vera :palette <id> [:r <r> :g <g> :b <b>])~%")
  (format t "(vera :sprite <id> :init)~%")
  (format t "(vera :sprite <id> <sprite_attribute settings>)~%")
  (format t "  Where <sprite_attribute settings> is one or more of the following:~%")
  (format t "  :tileset <t> :tile <i>~%")
  (format t "  :x <x> / :y <y> / :z <z> / :pal_offset <o> / :mask <mask>~%")
  (format t "  :hflip :enable|:disable / :vflip :enable|:disable~%")
  (format t "(vera :sprite <id> <sprite_attribute>)~%")
  (format t "  Where <sprite_attribute> is one of the following:~%")
  (format t "  :tileset :x :y :z :pal_offset :mask :hflip :vflip~%")
  (format t "(vera :sprite <id> [:info])~%")
  )

(defvar _vera_dispatch '(
  (:init . (lambda (arg_lookup iter) (vera_init)))
  (:map . _vera_map)
  (:tileset . _vera_tileset)
  (:pixel . _vera_pixel)
  (:linecapture . _vera_linecapture)
  (:spritebank . _vera_spritebank)
  (:ien . _vera_ien)
  (:isr . _vera_isr)
  (:irqline . _vera_irqline)
  (:scanline . (lambda (arg_lookup iter) (vera_scanline)))
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

(defun _vera (arg_lookup iter)
  (_dispatch _vera_dispatch arg_lookup iter))

(defun vera (&rest args)
  (if args
    (let ((atable (_make_assoc_table args)))
      (_vera (_make_assoc_table_lookup atable) atable))
    (_vera_help '())))

)lisplibrary";

