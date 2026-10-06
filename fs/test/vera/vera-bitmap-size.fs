<sheet-bitmap> bitmap0
<sheet-bitmap> bitmap1

: bitmap-size-test
  bitmap0 sheet{ 320 width 32 height 1 bpp }apply
  bitmap1 sheet{ 320 width 32 height 1 bpp }apply
  ." Bitmap size :" bitmap0 sheet-size@ hex. cr
  ." Bitmap 0 base: " bitmap0 sheet-base@ hex. cr
  ." Bitmap 1 base: " bitmap1 sheet-base@ hex. cr
;

[: bitmap-size-test ;] &>file tst_dir/vera-bitmap-size.log

s" tst_dir/vera-bitmap-size.log" s" vera-bitmap-size.ref" f_cmp ?assert

bitmap0 sheet-deinit
bitmap1 sheet-deinit
