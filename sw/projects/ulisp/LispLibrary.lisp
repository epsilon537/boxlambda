
;const char LispLibrary[] =  R"lisplibrary(

(defvar +vera-irq-vsync+ 1)
(defvar +vera-irq-line+ 2)
(defvar +vera-irq-sprcol+ 4)

(defvar +vera-scanline-visible-max+ 479)
(defvar +vera-scanline-max+ 524)

(defvar +vera-hstop-max+ 1023)
(defvar +vera-vstop-max+ 1023)

(defvar +vera-max-num-tiles-in-tileset+ 1024)

(defvar +vera-num-layers+ 2)

(defvar +vera-num-sprite-banks+ 2)
(defvar +vera-num-sprites-in-bank+ 64)
(defvar +vera-num-sprites+ (* +vera-num-sprite-banks+ +vera-num-sprites-in-bank+))
(defvar +vera-max-sprite-id+ 127)

(defvar +vera-num-maps+ 32)
(defvar +vera-num-tilesets+ 32)

(defvar +vera-color-black+ 0)
(defvar +vera-color-white+ 1)
(defvar +vera-color-red+ 2)
(defvar +vera-color-cyan+ 3)
(defvar +vera-color-purple+ 4)
(defvar +vera-color-green+ 5)
(defvar +vera-color-blue+ 6)
(defvar +vera-color-yellow+ 7)
(defvar +vera-color-orange+ 8)
(defvar +vera-color-brown+ 9)
(defvar +vera-color-light-red+ 10)
(defvar +vera-color-dark-grey+ 11)
(defvar +vera-color-grey+ 12)
(defvar +vera-color-light-green+ 13)
(defvar +vera-color-light-blue+ 14)
(defvar +vera-color-light-grey+ 15)

(defvar +vera-color-grayscale-0+ 16)
(defvar +vera-color-grayscale-1+ 17)
(defvar +vera-color-grayscale-2+ 18)
(defvar +vera-color-grayscale-3+ 19)
(defvar +vera-color-grayscale-4+ 20)
(defvar +vera-color-grayscale-5+ 21)
(defvar +vera-color-grayscale-6+ 22)
(defvar +vera-color-grayscale-7+ 23)
(defvar +vera-color-grayscale-8+ 24)
(defvar +vera-color-grayscale-9+ 25)
(defvar +vera-color-grayscale-10+ 26)
(defvar +vera-color-grayscale-11+ 27)
(defvar +vera-color-grayscale-12+ 28)
(defvar +vera-color-grayscale-13+ 29)
(defvar +vera-color-grayscale-14+ 30)
(defvar +vera-color-grayscale-15+ 31)

(defvar +vera-map-type-txt16+ 0)
(defvar +vera-map-type-txt256+ 1)
(defvar +vera-map-type-tile+ 2)

(defvar +vera-sprite-z-dis+ 0)
(defvar +vera-sprite-z-bg-l0+ 1)
(defvar +vera-sprite-z-l0-l1+ 2)
(defvar +vera-sprite-z-l1+ 3)

(defvar +vera-mapentry-hflip-mask+ 1024)
(defvar +vera-mapentry-vflip-mask+ 2048)
(defvar +vera-mapentry-pal-offset-shift+ 12)

(defun %make-assoc-table-lookup (table)
  (lambda (k &optional return-nil-on-miss)
    (let ((entry (assoc k table)))
      (if entry
        (cdr entry)
        (if return-nil-on-miss
          nil
          (error "Missing argument: ~a. Table: ~s" k table))))))

(defun %make-assoc-table (args)
  (defun %make-assoc-table-loop (table args)
    (if args
      (let ((key (first args)) (val (second args)))
        (if (numberp key) (error "Malformed argument list: ~a." args))
        (if (numberp val)
          (let ((table (cons (cons key val) table)) (args (cddr args)))
            (%make-assoc-table-loop table args))
          (let ((table (cons (cons key nil) table)) (args (cdr args)))
            (%make-assoc-table-loop table args))))
      (reverse table)))
  (%make-assoc-table-loop '() args))

(defun %last (l) (nth (1- (length l)) l))

(defun %dispatch (disp-table arg-lookup iter)
  (if iter
    (let* ((action (first (first iter)))
           (disp-tbl-entry (assoc action disp-table))
           (iter (rest iter)))
      (if disp-tbl-entry
        (funcall (cdr disp-tbl-entry) arg-lookup iter)
        (error "Action keyword not found: ~a Known keywords: ~a"
               action (mapcar first disp-table))))
    (funcall (cdr (%last disp-table)) arg-lookup iter)))

(defun %make-dispatcher (disp-table)
  (lambda (arg-lookup iter) (%dispatch disp-table arg-lookup iter)))

(defun %vera-ll-apply (f args &optional opt-arg)
  (let ((opt-val-list
          (let ((opt-val (arg-lookup opt-arg t)))
            (if opt-val (list opt-val))))
        (arg-list (mapcar arg-lookup args)))
    (apply f (append arg-list opt-val-list))))

(defun %vera-map-res (map)
  (mapcan (lambda (k v) (list k v)) '(:addr :width :height :map-type) map))

(defun %vera-map-init (arg-lookup iter)
  (%vera-map-res (%vera-ll-apply vera-map '(:map :width :height :map-type))))

(defvar %vera-map-dispatch (list
  (cons :width %vera-map-init)
  (cons :height %vera-map-init)
  (cons :map-type %vera-map-init)
  (cons :deinit (lambda (arg-lookup iter) (vera-map-deinit (arg-lookup :map))))
  (cons :entry (lambda (arg-lookup iter) (%vera-ll-apply vera-map-entry '(:map :x :y) :val)))
  (cons :info (lambda (arg-lookup iter) (%vera-map-res (vera-map (arg-lookup :map)))))))

(defun %vera-tileset-res (tileset)
  (mapcan (lambda (k v) (list k v))
            '(:addr :width :height :bpp :num-tiles :tilesize-bytes) tileset))

(defun %vera-tileset-init (arg-lookup iter)
  (%vera-tileset-res (%vera-ll-apply vera-tileset '(:tileset :width :height :bpp :num-tiles))))

(defvar %vera-tileset-dispatch (list
  (cons :width %vera-tileset-init)
  (cons :height %vera-tileset-init)
  (cons :bpp %vera-tileset-init)
  (cons :num-tiles %vera-tileset-init)
  (cons :deinit (lambda (arg-lookup iter) (vera-tileset-deinit (arg-lookup :tileset))))
  (cons :info (lambda (arg-lookup iter) (%vera-tileset-res (vera-tileset (arg-lookup :tileset)))))))

(defun %vera-pixel (arg-lookup iter)
  (%vera-ll-apply vera-tileset-pixel '(:tileset :tile :x :y) :val))

(defun %vera-linecapture-pixel-res (pixel)
  (mapcan (lambda (k v) (list k v)) '(:r :g :b) pixel))

(defun %vera-linecapture-pixel (arg-lookup iter)
  (%vera-linecapture-pixel-res (vera-linecapture-read-pixel (arg-lookup :x))))

(defvar %vera-linecapture-dispatch (list
  (cons :enable (lambda (arg-lookup iter) (vera-linecapture-enable 1)))
  (cons :disable (lambda (arg-lookup iter) (vera-linecapture-enable 0)))
  (cons :pixel %vera-linecapture-pixel)
  (cons :enabled (lambda (arg-lookup iter) (vera-linecapture-enable)))))

(defvar %vera-spritebank-dispatch (list
  (cons :select (lambda (arg-lookup iter) (vera-sprite-bank (arg-lookup :select))))
  (cons :selected (lambda (arg-lookup iter) (vera-sprite-bank)))))

(defvar %vera-ien-dispatch (list
  (cons :set (lambda (arg-lookup iter) (vera-ien (arg-lookup :mask) 1)))
  (cons :clr (lambda (arg-lookup iter) (vera-ien (arg-lookup :mask) 0)))
  (cons :get (lambda (arg-lookup iter) (vera-ien)))))

(defvar %vera-isr-dispatch (list
  (cons :set (lambda (arg-lookup iter) (vera-isr (arg-lookup :mask))))
  (cons :get (lambda (arg-lookup iter) (vera-isr)))))

(defvar %vera-display-dispatch (list
  (cons :enable (lambda (arg-lookup iter) (vera-display-enable 1)))
  (cons :disable (lambda (arg-lookup iter) (vera-display-enable 0)))
  (cons :enabled (lambda (arg-lookup iter) (vera-display-enable)))))

(defvar %vera-sprites-dispatch (list
  (cons :enable (lambda (arg-lookup iter) (vera-sprites-enable 1)))
  (cons :disable (lambda (arg-lookup iter) (vera-sprites-enable 0)))
  (cons :enabled (lambda (arg-lookup iter) (vera-sprites-enable)))))

(defun %vera-hscale (arg-lookup iter)
  (if iter (%vera arg-lookup iter))
  (%vera-ll-apply vera-hscale '() :hscale))

(defun %vera-vscale (arg-lookup iter)
  (if iter (%vera arg-lookup iter))
  (%vera-ll-apply vera-vscale '() :vscale))

(defun %vera-bordercolor (arg-lookup iter)
  (%vera-ll-apply vera-bordercolor '() :bordercolor))

(defun %vera-screen-boundaries-res (boundaries)
  (mapcan (lambda (k v) (list k v)) '(:hstart :hstop :vstart :vstop) boundaries))

(defun %vera-screen-boundaries (arg-lookup iter)
  (%vera-screen-boundaries-res
    (if (consp iter)
      (vera-screen-boundaries (arg-lookup :hstart) (arg-lookup :hstop) (arg-lookup :vstart) (arg-lookup :vstop))
      (vera-screen-boundaries))))

(defun %vera-layer-hscroll (arg-lookup iter)
  (if iter (%vera-layer arg-lookup iter))
  (%vera-ll-apply vera-layer-hscroll '(:layer) :hscroll))

(defun %vera-layer-vscroll (arg-lookup iter)
  (if iter (%vera-Layer arg-lookup iter))
  (%vera-ll-apply vera-layer-vscroll '(:layer) :vscroll))

(defun %vera-layer-map (arg-lookup iter)
  (if iter (%vera-layer arg-lookup iter))
  (%vera-ll-apply vera-layer-map '(:layer) :map))

(defun %vera-layer-tileset (arg-lookup iter)
  (if iter (%vera-layer arg-lookup iter))
  (%vera-ll-apply vera-layer-tileset '(:layer) :tileset))

(defun %vera-layer-bitmap-res (bitmap)
  (mapcan (lambda (k v) (list k v)) '(:tileset :tile) bitmap))

(defun %vera-layer-bitmap (arg-lookup iter)
  (%vera-layer-bitmap-res
    (let ((layer (arg-lookup :layer)) (tileset (arg-lookup :tileset t)) (tile (arg-lookup :tile t)))
      (if (and tileset tile)
        (vera-layer-bitmap layer tileset tile)
        (vera-layer-bitmap layer)))))

(defun %vera-layer-pal-offset (arg-lookup iter)
  (%vera-ll-apply vera-layer-pal-offset '(:layer) :pal-offset))

(defvar %vera-layer-dispatch (list
  (cons :enable (lambda (arg-lookup iter) (vera-layer-enable (arg-lookup :layer) 1)))
  (cons :disable (lambda (arg-lookup iter) (vera-layer-enable (arg-lookup :layer) 0)))
  (cons :enabled (lambda (arg-lookup iter) (vera-layer-enable (arg-lookup :layer))))
  (cons :hscroll %vera-layer-hscroll)
  (cons :vscroll %vera-layer-vscroll)
  (cons :map %vera-layer-map)
  (cons :tileset %vera-layer-tileset)
  (cons :bitmap %vera-layer-bitmap)
  (cons :pal-offset %vera-layer-pal-offset)))

(defvar %vera-layer (%make-dispatcher %vera-layer-dispatch))

(defun %vera-palette-res (palette)
  (mapcan (lambda (k v) (list k v)) '(:r :g :b) palette))

(defun %vera-palette (arg-lookup iter)
  (let ((idx (arg-lookup :palette t)))
    (if idx
      (%vera-palette-res
        (if (consp iter)
          (vera-palette idx (arg-lookup :r) (arg-lookup :g) (arg-lookup :b))
          (vera-palette idx)))
      (progn
        (arg-lookup :restore)
        (vera-palette)))))

(defun %vera-sprite-tileset-res (tileset)
  (mapcan (lambda (k v) (list k v)) '(:tileset :tile) tileset))

(defun %vera-sprite-tileset (arg-lookup iter)
  (%vera-sprite-tileset-res
    (let ((id (arg-lookup :sprite)) (tileset (arg-lookup :tileset t)) (tile (arg-lookup :tile t)))
      (if (and tileset tile)
        (progn
          (vera-sprite-tile id tileset tile)
          (if (cdr iter) (%vera-sprite arg-lookup (cdr iter))))
        (vera-sprite-tile id)))))

(defun %vera-sprite-x (arg-lookup iter)
  (if iter (%vera-sprite arg-lookup iter))
  (%vera-ll-apply vera-sprite-x '(:sprite) :x))

(defun %vera-sprite-y (arg-lookup iter)
  (if iter (%vera-sprite arg-lookup iter))
  (%vera-ll-apply vera-sprite-y '(:sprite) :y))

(defun %vera-sprite-z (arg-lookup iter)
  (if iter (%vera-sprite arg-lookup iter))
  (%vera-ll-apply vera-sprite-z-depth '(:sprite) :z))

(defun %vera-sprite-pal-offset (arg-lookup iter)
  (if iter (%vera-sprite arg-lookup iter))
  (%vera-ll-apply vera-sprite-pal-offset '(:sprite) :pal-offset))

(defun %vera-sprite-mask (arg-lookup iter)
  (if iter (%vera-sprite arg-lookup iter))
  (%vera-ll-apply vera-sprite-mask '(:sprite) :mask))

(defun %vera-sprite-hflip-enable (arg-lookup iter)
  (if iter (%vera-sprite arg-lookup iter))
  (vera-sprite-hflip (arg-lookup :sprite) 1))

(defun %vera-sprite-hflip-disable (arg-lookup iter)
  (if iter (%vera-sprite arg-lookup iter))
  (vera-sprite-hflip (arg-lookup :sprite) 0))

(defvar %vera-sprite-hflip-dispatch (list
  (cons :enable %vera-sprite-hflip-enable)
  (cons :disable %vera-sprite-hflip-disable)
  (cons :enabled (lambda (arg-lookup iter) (vera-sprite-hflip (arg-lookup :sprite))))))

(defun %vera-sprite-vflip-enable (arg-lookup iter)
  (if iter (%vera-sprite arg-lookup iter))
  (vera-sprite-vflip (arg-lookup :sprite) 1))

(defun %vera-sprite-vflip-disable (arg-lookup iter)
  (if iter (%vera-sprite arg-lookup iter))
  (vera-sprite-vflip (arg-lookup :sprite) 0))

(defvar %vera-sprite-vflip-dispatch (list
  (cons :enable %vera-sprite-vflip-enable)
  (cons :disable %vera-sprite-vflip-disable)
  (cons :enabled (lambda (arg-lookup iter) (vera-sprite-vflip (arg-lookup :sprite))))))

(defun %vera-sprite-info (arg-lookup iter)
  (let* ((id (arg-lookup :sprite))
         (tileset-info (vera-sprite-tile id))
         (tileset (if tileset-info (first tileset-info) '()))
         (tile (if tileset-info (second tileset-info) '()))
         (x (vera-sprite-x id))
         (y (vera-sprite-y id))
         (z (vera-sprite-z-depth id))
         (pal-offset (vera-sprite-pal-offset id))
         (mask (vera-sprite-mask id))
         (hflip (if (= 1 (vera-sprite-hflip id)) :enabled :disabled))
         (vflip (if (= 1 (vera-sprite-vflip id)) :enabled :disabled)))
    (list :tileset tileset :tile tile :x x :y y :z z :pal-offset pal-offset :mask mask :hflip hflip :vflip vflip)))

(defvar %vera-sprite-dispatch (list
  (cons :init (lambda (arg-lookup iter) (vera-sprite-init (arg-lookup :sprite))))
  (cons :tileset %vera-sprite-tileset)
  (cons :x %vera-sprite-x)
  (cons :y %vera-sprite-y)
  (cons :z %vera-sprite-z)
  (cons :pal-offset %vera-sprite-pal-offset)
  (cons :mask %vera-sprite-mask)
  (cons :hflip (%make-dispatcher %vera-sprite-hflip-dispatch))
  (cons :vflip (%make-dispatcher %vera-sprite-vflip-dispatch))
  (cons :info %vera-sprite-info)))

(defvar %vera-sprite (%make-dispatcher %vera-sprite-dispatch))

(defvar %vera-dispatch (list
  (cons :init (lambda (arg-lookup iter) (vera-init)))
  (cons :map (%make-dispatcher %vera-map-dispatch))
  (cons :tileset (%make-dispatcher %vera-tileset-dispatch))
  (cons :pixel %vera-pixel)
  (cons :linecapture (%make-dispatcher %vera-linecapture-dispatch))
  (cons :spritebank (%make-dispatcher %vera-spritebank-dispatch))
  (cons :ien (%make-dispatcher %vera-ien-dispatch))
  (cons :isr (%make-dispatcher %vera-isr-dispatch))
  (cons :irqline (lambda (arg-lookup iter) (%vera-ll-apply vera-irqline '() :irqline)))
  (cons :scanline (lambda (arg-lookup iter) (vera-scanline)))
  (cons :display (%make-dispatcher %vera-display-dispatch))
  (cons :sprites (%make-dispatcher %vera-sprites-dispatch))
  (cons :hscale %vera-hscale)
  (cons :vscale %vera-vscale)
  (cons :bordercolor %vera-bordercolor)
  (cons :boundaries %vera-screen-boundaries)
  (cons :layer %vera-layer)
  (cons :palette %vera-palette)
  (cons :sprite %vera-sprite)))

(defun %vera (arg-lookup iter)
  (%dispatch %vera-dispatch arg-lookup iter))

(defun vera (&rest args)
  "Vera graphics API. General form: (vera :<action> [:<param keyword> [param value]] ...

(vera :init)
(vera :map <id> :width <w> :height <h> :map-type <t>)
(vera :map <id> :deinit)
(vera :map <id> :entry :x <x> :y <y> [:val <v>])
(vera :map <id> [:info])
(vera :tileset <id> :width <w> :height <h> :bpp <b> :num-tiles <n>)
(vera :tileset <id> :deinit)
(vera :tileset <id> [:info])
(vera :pixel :tileset <id> :tile <idx> :x <x> :y <y> [:val <v>])
(vera :linecapture :enable|:disable|:enabled)
(vera :linecapture :pixel :x <x>)
(vera :spritebank :select <idx>)
(vera :spritebank :selected)
(vera :ien :set|:clr :mask <m>)
(vera :ien :get)
(vera :isr :set :mask <m>)
(vera :isr :get)
(vera :irqline [<l>])
(vera :scanline)
(vera :display :enable|:disable|:enabled)
(vera :sprites :enable|:disable|:enabled)
(vera [:hscale <h>] [:vscale <v>])
(vera :hscale|:vscale)
(vera :bordercolor [<c>])
(vera :boundaries [:hstart <hs> :hstop <he> :vstart <vs> :vstop <ve>])
(vera :layer <id> :enable|:disable|:enabled)
(vera :layer <id> [:hscroll <h>] [:vscroll <v>])
(vera :layer <id> :hscroll|:vscroll)
(vera :layer <id> :map [<m>] [:tileset t])
(vera :layer <id> :tileset [<t>] [:map <m>])
(vera :layer <id> :bitmap [:tileset <t> :tile <i>])
(vera :layer <id> :pal-offset [<o>])
(vera :palette :restore)
(vera :palette <id> [:r <r> :g <g> :b <b>])
(vera :sprite <id> :init)
(vera :sprite <id> <sprite-attribute settings>)
  Where <sprite-attribute settings> is one or more of the following:
  :tileset <t> :tile <i>
  :x <x> / :y <y> / :z <z> / :pal-offset <o> / :mask <mask>
  :hflip :enable|:disable / :vflip :enable|:disable
(vera :sprite <id> <sprite-attribute>)
  Where <sprite-attribute> is one of the following:
  :tileset :x :y :z :pal-offset :mask :hflip :vflip
(vera :sprite <id> [:info])
"
  (if args
    (let ((atable (%make-assoc-table args)))
      (%vera (%make-assoc-table-lookup atable) atable))
    (? vera)))

)lisplibrary";

