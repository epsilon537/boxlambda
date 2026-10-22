\ --- DFX_CTRL
\ --- 32-bit register bitfield accessors

$10000400 constant DFX_CTRL_BASE_ADDR


\ CONTROL - Control register.
DFX_CTRL_BASE_ADDR $0 + constant DFX_CTRL_CONTROL_ADDR

\ -- Address Mask Offset
DFX_CTRL_CONTROL_ADDR $ff #0 bitfield@ DFX_CTRL_CONTROL_CMD@
DFX_CTRL_CONTROL_ADDR $ff #0 bitfield! DFX_CTRL_CONTROL_CMD!

  #0 constant DFX_CTRL_CONTROL_CMD_SHUTDOWN \ Shutdown. 
  #1 constant DFX_CTRL_CONTROL_CMD_RESTART_NO_STAT \ Restart without status. 
  #2 constant DFX_CTRL_CONTROL_CMD_RESTART_STAT \ Restart with status. 
  #3 constant DFX_CTRL_CONTROL_CMD_PROCEED \ Proceed. 
  #4 constant DFX_CTRL_CONTROL_CMD_USR_CTRL \ Proceed. 

DFX_CTRL_CONTROL_ADDR $ff00 #8 bitfield@ DFX_CTRL_CONTROL_BYTE@
DFX_CTRL_CONTROL_ADDR $ff00 #8 bitfield! DFX_CTRL_CONTROL_BYTE!
DFX_CTRL_CONTROL_ADDR $ffff0000 #16 bitfield@ DFX_CTRL_CONTROL_HALFWORD@
DFX_CTRL_CONTROL_ADDR $ffff0000 #16 bitfield! DFX_CTRL_CONTROL_HALFWORD!
