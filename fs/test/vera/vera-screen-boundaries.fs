: screen-boundaries-test

  true display-enable
  false sprites-enable
  false l0 layer-enable
  false l1 layer-enable

  ." bordercolor: " cr
  BLUE bordercolor!
  bordercolor@ . cr

  ." boundaries: " cr
  #100 #540 #100 #380 boundaries!
  boundaries@
  . cr . cr . cr . cr

  #99 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." line capture y=99: " cr
  #100 line-capture-pxl@ hex. cr
  #539 line-capture-pxl@ hex. cr
  cr

  #100 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." line capture y=100: " cr
  #99 line-capture-pxl@ hex. cr
  #100 line-capture-pxl@ hex. cr
  #539 line-capture-pxl@ hex. cr
  #540 line-capture-pxl@ hex. cr
  cr

  #379 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." line capture y=379: " cr
  #99 line-capture-pxl@ hex. cr
  #100 line-capture-pxl@ hex. cr
  #539 line-capture-pxl@ hex. cr
  #540 line-capture-pxl@ hex. cr

  #380 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." line capture y=380: " cr
  #100 line-capture-pxl@ hex. cr
  #539 line-capture-pxl@ hex. cr
;

[: screen-boundaries-test ;] &>file tst_dir/vera-screen-boundaries.log

s" tst_dir/vera-screen-boundaries.log" s" vera-screen-boundaries.ref" f_cmp ?assert

