
<tset> ts
1 <spr> spr1
2 <spr> spr2

: sprite-collision

  true display-enable
  true sprites-enable
  false l0 layer-enable
  false l1 layer-enable

  ts tset{ #64 width #64 height 8 bpp 8 tiles }set
  ts tset-print
  spr1 spr{ ts tset 2 tidx SPR-L1 z 0 paloffset #40 #50 vec2 xy 1 colmask }set
  spr2 spr{ ts tset 2 tidx SPR-L1 z 0 paloffset #200 #200 vec2 xy 1 colmask }set

  64 0 do
    64 i 1+ min 0 ?do
      ts pxl{ 2 tidx i j vec2 xy CYAN color }set
    loop
  loop

  ." spr1 colmask: " spr1 spr-colmask@ . cr
  ." spr2 colmask: " spr2 spr-colmask@ . cr
  -1 irq-ack
  0 irqline!

  IRQ-SPRCOL-MASK irq-enable
  IRQ-LINE-MASK irq-enable

  ." Waiting full frame..." cr

  begin irq-get IRQ-LINE-MASK and until
  IRQ-LINE-MASK irq-ack
  irq-get IRQ-LINE-MASK and 0= ?assert
  begin irq-get IRQ-LINE-MASK and until
  IRQ-LINE-MASK irq-ack

  ." Checking sprcol irq... "
  irq-get dup . cr
  IRQ-SPRCOL-MASK and 0= ?assert
 
  ." Moving spr2 to collision..." cr
  spr2 spr{ ts tset 2 tidx SPR-L1 z 0 paloffset #40 #50 vec2 xy 1 colmask }set

  ." Waiting for sprcol irq..." cr
  begin irq-get IRQ-SPRCOL-MASK and until
  IRQ-SPRCOL-MASK irq-ack
  ." Sprite collision detected." cr
;

[: sprite-collision ;] &>file tst_dir/vera-sprite-collision.log

s" tst_dir/vera-sprite-collision.log" s" vera-sprite-collision.ref" f_cmp ?assert

ts tset-deinit

