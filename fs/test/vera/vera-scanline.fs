: vera-scanline-test

  begin scanline@ 10 < until
  begin scanline@ 10 >= until
  scanline@
  scanline@
  scanline@ ( l0 l1 l2 )
  rot dup 10 > . cr ( l1 l2 l0 )
  -rot over > . cr ( l0 l1 )
  < . cr
;

[: vera-scanline-test ;] &>file tst_dir/vera-scanline.log

s" tst_dir/vera-scanline.log" s" vera-scanline.ref" f_cmp ?assert

