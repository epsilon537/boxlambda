
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

(defun _make_dispatcher (disp_table)
  (lambda (arg_lookup iter) (_dispatch disp_table arg_lookup iter)))

(defun _vera_ll_apply (f args &optional opt_arg)
  (let ((opt_val_list
          (let ((opt_val (arg_lookup opt_arg t)))
            (if opt_val (list opt_val))))
        (arg_list (mapcar arg_lookup args)))
    (apply f (append arg_list opt_val_list))))

(defun _vera_map_res (map)
  (mapcan (lambda (k v) (list k v)) '(:addr :width :height :map_type) map))

(defun _vera_map_init (arg_lookup iter)
  (_vera_map_res (_vera_ll_apply vera_map '(:map :width :height :map_type))))

(defvar _vera_map_dispatch (list
  (cons :width _vera_map_init)
  (cons :height _vera_map_init)
  (cons :map_type _vera_map_init)
  (cons :deinit (lambda (arg_lookup iter) (vera_map_deinit (arg_lookup :map))))
  (cons :entry (lambda (arg_lookup iter) (_vera_ll_apply vera_map_entry '(:map :x :y) :val)))
  (cons :info (lambda (arg_lookup iter) (_vera_map_res (vera_map (arg_lookup :map)))))))

(defun _vera_tileset_res (tileset)
  (mapcan (lambda (k v) (list k v))
            '(:addr :width :height :bpp :num_tiles :tilesize_bytes) tileset))

(defun _vera_tileset_init (arg_lookup iter)
  (_vera_tileset_res (_vera_ll_apply vera_tileset '(:tileset :width :height :bpp :num_tiles))))

(defvar _vera_tileset_dispatch (list
  (cons :width _vera_tileset_init)
  (cons :height _vera_tileset_init)
  (cons :bpp _vera_tileset_init)
  (cons :num_tiles _vera_tileset_init)
  (cons :deinit (lambda (arg_lookup iter) (vera_tileset_deinit (arg_lookup :tileset))))
  (cons :info (lambda (arg_lookup iter) (_vera_tileset_res (vera_tileset (arg_lookup :tileset)))))))

(defun _vera_pixel (arg_lookup iter)
  (_vera_ll_apply vera_tileset_pixel '(:tileset :tile :x :y) :val))

(defun _vera_linecapture_pixel_res (pixel)
  (mapcan (lambda (k v) (list k v)) '(:r :g :b) pixel))

(defun _vera_linecapture_pixel (arg_lookup iter)
  (_vera_linecapture_pixel_res (vera_line_capture_read_pixel (arg_lookup :x))))

(defvar _vera_linecapture_dispatch (list
  (cons :enable (lambda (arg_lookup iter) (vera_line_capture_enable 1)))
  (cons :disable (lambda (arg_lookup iter) (vera_line_capture_enable 0)))
  (cons :pixel _vera_linecapture_pixel)
  (cons :enabled (lambda (arg_lookup iter) (vera_line_capture_enable)))))

(defvar _vera_spritebank_dispatch (list
  (cons :select (lambda (arg_lookup iter) (vera_sprite_bank (arg_lookup :select))))
  (cons :selected (lambda (arg_lookup iter) (vera_sprite_bank)))))

(defvar _vera_ien_dispatch (list
  (cons :set (lambda (arg_lookup iter) (vera_ien (arg_lookup :mask) 1)))
  (cons :clr (lambda (arg_lookup iter) (vera_ien (arg_lookup :mask) 0)))
  (cons :get (lambda (arg_lookup iter) (vera_ien)))))

(defvar _vera_isr_dispatch (list
  (cons :set (lambda (arg_lookup iter) (vera_isr (arg_lookup :mask))))
  (cons :get (lambda (arg_lookup iter) (vera_isr)))))

(defvar _vera_display_dispatch (list
  (cons :enable (lambda (arg_lookup iter) (vera_display_enable 1)))
  (cons :disable (lambda (arg_lookup iter) (vera_display_enable 0)))
  (cons :enabled (lambda (arg_lookup iter) (vera_display_enable)))))

(defvar _vera_sprites_dispatch (list
  (cons :enable (lambda (arg_lookup iter) (vera_sprites_enable 1)))
  (cons :disable (lambda (arg_lookup iter) (vera_sprites_enable 0)))
  (cons :enabled (lambda (arg_lookup iter) (vera_sprites_enable)))))

(defun _vera_hscale (arg_lookup iter)
  (if iter (_vera arg_lookup iter))
  (_vera_ll_apply vera_hscale '() :hscale))

(defun _vera_vscale (arg_lookup iter)
  (if iter (_vera arg_lookup iter))
  (_vera_ll_apply vera_vscale '() :vscale))

(defun _vera_bordercolor (arg_lookup iter)
  (_vera_ll_apply vera_bordercolor '() :bordercolor))

(defun _vera_screen_boundaries_res (boundaries)
  (mapcan (lambda (k v) (list k v)) '(:hstart :hstop :vstart :vstop) boundaries))

(defun _vera_screen_boundaries (arg_lookup iter)
  (_vera_screen_boundaries_res
    (if (consp iter)
      (vera_screen_boundaries (arg_lookup :hstart) (arg_lookup :hstop) (arg_lookup :vstart) (arg_lookup :vstop))
      (vera_screen_boundaries))))

(defun _vera_layer_hscroll (arg_lookup iter)
  (if iter (_vera_layer arg_lookup iter))
  (_vera_ll_apply vera_layer_hscroll '(:layer) :hscroll))

(defun _vera_layer_vscroll (arg_lookup iter)
  (if iter (_vera_Layer arg_lookup iter))
  (_vera_ll_apply vera_layer_vscroll '(:layer) :vscroll))

(defun _vera_layer_map (arg_lookup iter)
  (if iter (_vera_layer arg_lookup iter))
  (_vera_ll_apply vera_layer_map '(:layer) :map))

(defun _vera_layer_tileset (arg_lookup iter)
  (if iter (_vera_layer arg_lookup iter))
  (_vera_ll_apply vera_layer_tileset '(:layer) :tileset))

(defun _vera_layer_bitmap_res (bitmap)
  (mapcan (lambda (k v) (list k v)) '(:tileset :tile) bitmap))

(defun _vera_layer_bitmap (arg_lookup iter)
  (_vera_layer_bitmap_res
    (let ((layer (arg_lookup :layer)) (tileset (arg_lookup :tileset t)) (tile (arg_lookup :tile t)))
      (if (and tileset tile)
        (vera_layer_bitmap layer tileset tile)
        (vera_layer_bitmap layer)))))

(defun _vera_layer_pal_offset (arg_lookup iter)
  (_vera_ll_apply vera_layer_pal_offset '(:layer) :pal_offset))

(defvar _vera_layer_dispatch (list
  (cons :enable (lambda (arg_lookup iter) (vera_layer_enable (arg_lookup :layer) 1)))
  (cons :disable (lambda (arg_lookup iter) (vera_layer_enable (arg_lookup :layer) 0)))
  (cons :enabled (lambda (arg_lookup iter) (vera_layer_enable (arg_lookup :layer))))
  (cons :hscroll _vera_layer_hscroll)
  (cons :vscroll _vera_layer_vscroll)
  (cons :map _vera_layer_map)
  (cons :tileset _vera_layer_tileset)
  (cons :bitmap _vera_layer_bitmap)
  (cons :pal_offset _vera_layer_pal_offset)))

(defvar _vera_layer (_make_dispatcher _vera_layer_dispatch))

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
  (if iter (_vera_sprite arg_lookup iter))
  (_vera_ll_apply vera_sprite_x '(:sprite) :x))

(defun _vera_sprite_y (arg_lookup iter)
  (if iter (_vera_sprite arg_lookup iter))
  (_vera_ll_apply vera_sprite_y '(:sprite) :y))

(defun _vera_sprite_z (arg_lookup iter)
  (if iter (_vera_sprite arg_lookup iter))
  (_vera_ll_apply vera_sprite_z_depth '(:sprite) :z))

(defun _vera_sprite_pal_offset (arg_lookup iter)
  (if iter (_vera_sprite arg_lookup iter))
  (_vera_ll_apply vera_sprite_pal_offset '(:sprite) :pal_offset))

(defun _vera_sprite_mask (arg_lookup iter)
  (if iter (_vera_sprite arg_lookup iter))
  (_vera_ll_apply vera_sprite_col_mask '(:sprite) :mask))

(defun _vera_sprite_hflip_enable (arg_lookup iter)
  (if iter (_vera_sprite arg_lookup iter))
  (vera_sprite_hflip (arg_lookup :sprite) 1))

(defun _vera_sprite_hflip_disable (arg_lookup iter)
  (if iter (_vera_sprite arg_lookup iter))
  (vera_sprite_hflip (arg_lookup :sprite) 0))

(defvar _vera_sprite_hflip_dispatch (list
  (cons :enable _vera_sprite_hflip_enable)
  (cons :disable _vera_sprite_hflip_disable)
  (cons :enabled (lambda (arg_lookup iter) (vera_sprite_hflip (arg_lookup :sprite))))))

(defun _vera_sprite_vflip_enable (arg_lookup iter)
  (if iter (_vera_sprite arg_lookup iter))
  (vera_sprite_vflip (arg_lookup :sprite) 1))

(defun _vera_sprite_vflip_disable (arg_lookup iter)
  (if iter (_vera_sprite arg_lookup iter))
  (vera_sprite_vflip (arg_lookup :sprite) 0))

(defvar _vera_sprite_vflip_dispatch (list
  (cons :enable _vera_sprite_vflip_enable)
  (cons :disable _vera_sprite_vflip_disable)
  (cons :enabled (lambda (arg_lookup iter) (vera_sprite_vflip (arg_lookup :sprite))))))

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

(defvar _vera_sprite_dispatch (list
  (cons :init (lambda (arg_lookup iter) (vera_sprite_init (arg_lookup :sprite))))
  (cons :tileset _vera_sprite_tileset)
  (cons :x _vera_sprite_x)
  (cons :y _vera_sprite_y)
  (cons :z _vera_sprite_z)
  (cons :pal_offset _vera_sprite_pal_offset)
  (cons :mask _vera_sprite_mask)
  (cons :hflip (_make_dispatcher _vera_sprite_hflip_dispatch))
  (cons :vflip (_make_dispatcher _vera_sprite_vflip_dispatch))
  (cons :info _vera_sprite_info)))

(defvar _vera_sprite (_make_dispatcher _vera_sprite_dispatch))

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

(defvar _vera_dispatch (list
  (cons :init (lambda (arg_lookup iter) (vera_init)))
  (cons :map (_make_dispatcher _vera_map_dispatch))
  (cons :tileset (_make_dispatcher _vera_tileset_dispatch))
  (cons :pixel _vera_pixel)
  (cons :linecapture (_make_dispatcher _vera_linecapture_dispatch))
  (cons :spritebank (_make_dispatcher _vera_spritebank_dispatch))
  (cons :ien (_make_dispatcher _vera_ien_dispatch))
  (cons :isr (_make_dispatcher _vera_isr_dispatch))
  (cons :irqline (lambda (arg_lookup iter) (_vera_ll_apply vera_irqline '() :irqline)))
  (cons :scanline (lambda (arg_lookup iter) (vera_scanline)))
  (cons :display (_make_dispatcher _vera_display_dispatch))
  (cons :sprites (_make_dispatcher _vera_sprites_dispatch))
  (cons :hscale _vera_hscale)
  (cons :vscale _vera_vscale)
  (cons :bordercolor _vera_bordercolor)
  (cons :boundaries _vera_screen_boundaries)
  (cons :layer _vera_layer)
  (cons :palette _vera_palette)
  (cons :sprite _vera_sprite)
  (cons :help _vera_help)))

(defun _vera (arg_lookup iter)
  (_dispatch _vera_dispatch arg_lookup iter))

(defun vera (&rest args)
  (if args
    (let ((atable (_make_assoc_table args)))
      (_vera (_make_assoc_table_lookup atable) atable))
    (_vera_help '())))

)lisplibrary";

