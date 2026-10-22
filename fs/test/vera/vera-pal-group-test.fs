: pal-group-test
  cr
  $123 $456 $789 $abc $def $fed $cba $987 $654 $321 $111 $222 $333 $444 $555 $666 4 
  pal-group!
  4 pal-group@

  ." 4 pal-group@ res: " cr
  16 0 do 
    hex. cr
  loop

  ." pal[16*4+i]:" cr
  16 0 do 
    #16 4 * i + pal@ hex. cr
  loop

  ." 4 5 pal-group-1:" cr
  $d0d 4 5 pal-group-1!
  4 5 pal-group-1@ hex. cr
  #16 4 * 5 + pal@ hex. cr

  ." 70 pal-abs>pal-group: "
  70 pal-abs>pal-group . . cr

  ." Stack should be empty now. depth: " depth . cr
;

[: pal-group-test ;] &>file tst_dir/vera-pal-group-test.log

s" tst_dir/vera-pal-group-test.log" s" vera-pal-group-test.ref" f_cmp ?assert

