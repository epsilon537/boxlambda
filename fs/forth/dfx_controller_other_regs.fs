\ --- DFX
\ --- 32-bit register bitfield accessors

$10000400 constant DFX_BASE_ADDR


\ STATUS - Status register.
DFX_BASE_ADDR $0 + constant DFX_STATUS_ADDR

\ -- Address Mask Offset
DFX_STATUS_ADDR $7 #0 bitfield@ DFX_STATUS_STATE@

  #0 constant DFX_STATUS_STATE_VS_EMPTY \ Virtual socket empty. 
  #1 constant DFX_STATUS_STATE_HW_SHUTDOWN \ Hardware shutdown step. 
  #2 constant DFX_STATUS_STATE_SW_SHUTDOWN \ Executing software shutdown step. 
  #4 constant DFX_STATUS_STATE_LOADING \ Loading the new reconfigurable module. 
  #5 constant DFX_STATUS_STATE_SW_STARTUP \ Executing software startup step. 
  #6 constant DFX_STATUS_STATE_RM_RESET \ Executing reconfigurable module reset step. 
  #7 constant DFX_STATUS_STATE_VS_FULL \ Virtual Socket is full. 

DFX_STATUS_ADDR $78 #3 bitfield@ DFX_STATUS_ERR@

  #0 constant DFX_STATUS_ERR_NO_ERR \ No error. 
  #1 constant DFX_STATUS_ERR_BAD_CONFIG \ Bad configuration error. 
  #2 constant DFX_STATUS_ERR_BS \ Bitstream error. 
  #4 constant DFX_STATUS_ERR_FETCH \ Fetch error. 
  #5 constant DFX_STATUS_ERR_BS_FETCH \ Bitstream and fetch error. 
  #7 constant DFX_STATUS_ERR_BAD_SIZE \ Bad size error. 
  #8 constant DFX_STATUS_ERR_BAD_FORMAT \ Bad format error. 
  #15 constant DFX_STATUS_ERR_UNKNOWN \ Unknown error. 

DFX_STATUS_ADDR $80 #7 bitfield@ DFX_STATUS_SHUTDOWN@
DFX_STATUS_ADDR $ffff00 #8 bitfield@ DFX_STATUS_RM_ID@

\ SW_TRIGGER - Software trigger register.
DFX_BASE_ADDR $4 + constant DFX_SW_TRIGGER_ADDR

\ -- Address Mask Offset
DFX_SW_TRIGGER_ADDR $1 #0 bitfield@ DFX_SW_TRIGGER_TRIGGER_ID@
DFX_SW_TRIGGER_ADDR $1 #0 bitfield! DFX_SW_TRIGGER_TRIGGER_ID!
DFX_SW_TRIGGER_ADDR $80000000 #31 bitfield@ DFX_SW_TRIGGER_TRIGGER_PENDING@

\ TRIGGER_0 - Trigger 0.
DFX_BASE_ADDR $20 + constant DFX_TRIGGER_0_ADDR

\ -- Address Mask Offset
DFX_TRIGGER_0_ADDR $ffffffff #0 bitfield@ DFX_TRIGGER_0_VALUE@
DFX_TRIGGER_0_ADDR $ffffffff #0 bitfield! DFX_TRIGGER_0_VALUE!

\ TRIGGER_1 - Trigger 1.
DFX_BASE_ADDR $24 + constant DFX_TRIGGER_1_ADDR

\ -- Address Mask Offset
DFX_TRIGGER_1_ADDR $ffffffff #0 bitfield@ DFX_TRIGGER_1_VALUE@
DFX_TRIGGER_1_ADDR $ffffffff #0 bitfield! DFX_TRIGGER_1_VALUE!

\ RM_BS_INDEX_0 - Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 0.
DFX_BASE_ADDR $40 + constant DFX_RM_BS_INDEX_0_ADDR

\ -- Address Mask Offset
DFX_RM_BS_INDEX_0_ADDR $ffff #0 bitfield@ DFX_RM_BS_INDEX_0_INDEX@
DFX_RM_BS_INDEX_0_ADDR $ffff #0 bitfield! DFX_RM_BS_INDEX_0_INDEX!

\ RM_CONTROL_0 - Control info for Reconfigurable Module 0.
DFX_BASE_ADDR $44 + constant DFX_RM_CONTROL_0_ADDR

\ -- Address Mask Offset
DFX_RM_CONTROL_0_ADDR $3 #0 bitfield@ DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED@
DFX_RM_CONTROL_0_ADDR $3 #0 bitfield! DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED!

  #0 constant DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_NO_SHUTDOWN \ No shutdown required. 
  #1 constant DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_HW_RM \ Hardware Reconfigurable Module shutdown required. 
  #2 constant DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_HW_THEN_SW \ Hardware then software shutdown required. 
  #3 constant DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_SW_THEN_HW \ Software then hardware shutdown required. 

DFX_RM_CONTROL_0_ADDR $4 #2 bitfield@ DFX_RM_CONTROL_0_STARTUP_REQUIRED@
DFX_RM_CONTROL_0_ADDR $4 #2 bitfield! DFX_RM_CONTROL_0_STARTUP_REQUIRED!
DFX_RM_CONTROL_0_ADDR $18 #3 bitfield@ DFX_RM_CONTROL_0_RST_REQUIRED@
DFX_RM_CONTROL_0_ADDR $18 #3 bitfield! DFX_RM_CONTROL_0_RST_REQUIRED!

  #0 constant DFX_RM_CONTROL_0_RST_REQUIRED_NO_RST \ No reset required. 
  #2 constant DFX_RM_CONTROL_0_RST_REQUIRED_ACTIVE_LO_RST \ Active low reset required. 
  #3 constant DFX_RM_CONTROL_0_RST_REQUIRED_ACTIVE_HI_RST \ Active high reset required. 

DFX_RM_CONTROL_0_ADDR $1fe0 #5 bitfield@ DFX_RM_CONTROL_0_RST_DURATION@
DFX_RM_CONTROL_0_ADDR $1fe0 #5 bitfield! DFX_RM_CONTROL_0_RST_DURATION!

\ RM_BS_INDEX_1 - Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 1.
DFX_BASE_ADDR $48 + constant DFX_RM_BS_INDEX_1_ADDR

\ -- Address Mask Offset
DFX_RM_BS_INDEX_1_ADDR $ffff #0 bitfield@ DFX_RM_BS_INDEX_1_INDEX@
DFX_RM_BS_INDEX_1_ADDR $ffff #0 bitfield! DFX_RM_BS_INDEX_1_INDEX!

\ RM_CONTROL_1 - Control info for Reconfigurable Module 1.
DFX_BASE_ADDR $4c + constant DFX_RM_CONTROL_1_ADDR

\ -- Address Mask Offset
DFX_RM_CONTROL_1_ADDR $3 #0 bitfield@ DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED@
DFX_RM_CONTROL_1_ADDR $3 #0 bitfield! DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED!

  #0 constant DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_NO_SHUTDOWN \ No shutdown required. 
  #1 constant DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_HW_RM \ Hardware Reconfigurable Module shutdown required. 
  #2 constant DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_HW_THEN_SW \ Hardware then software shutdown required. 
  #3 constant DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_SW_THEN_HW \ Software then hardware shutdown required. 

DFX_RM_CONTROL_1_ADDR $4 #2 bitfield@ DFX_RM_CONTROL_1_STARTUP_REQUIRED@
DFX_RM_CONTROL_1_ADDR $4 #2 bitfield! DFX_RM_CONTROL_1_STARTUP_REQUIRED!
DFX_RM_CONTROL_1_ADDR $18 #3 bitfield@ DFX_RM_CONTROL_1_RST_REQUIRED@
DFX_RM_CONTROL_1_ADDR $18 #3 bitfield! DFX_RM_CONTROL_1_RST_REQUIRED!

  #0 constant DFX_RM_CONTROL_1_RST_REQUIRED_NO_RST \ No reset required. 
  #2 constant DFX_RM_CONTROL_1_RST_REQUIRED_ACTIVE_LO_RST \ Active low reset required. 
  #3 constant DFX_RM_CONTROL_1_RST_REQUIRED_ACTIVE_HI_RST \ Active high reset required. 

DFX_RM_CONTROL_1_ADDR $1fe0 #5 bitfield@ DFX_RM_CONTROL_1_RST_DURATION@
DFX_RM_CONTROL_1_ADDR $1fe0 #5 bitfield! DFX_RM_CONTROL_1_RST_DURATION!

\ BS_ID_0 - Bitstream 0 ID.
DFX_BASE_ADDR $60 + constant DFX_BS_ID_0_ADDR

\ -- Address Mask Offset
DFX_BS_ID_0_ADDR $ffffffff #0 bitfield@ DFX_BS_ID_0_VALUE@
DFX_BS_ID_0_ADDR $ffffffff #0 bitfield! DFX_BS_ID_0_VALUE!

\ BS_ADDRESS_0 - Bitstream 0 byte address.
DFX_BASE_ADDR $64 + constant DFX_BS_ADDRESS_0_ADDR

\ -- Address Mask Offset
DFX_BS_ADDRESS_0_ADDR $ffffffff #0 bitfield@ DFX_BS_ADDRESS_0_VALUE@
DFX_BS_ADDRESS_0_ADDR $ffffffff #0 bitfield! DFX_BS_ADDRESS_0_VALUE!

\ BS_SIZE_0 - Bitstream 0 size in bytes.
DFX_BASE_ADDR $68 + constant DFX_BS_SIZE_0_ADDR

\ -- Address Mask Offset
DFX_BS_SIZE_0_ADDR $ffffffff #0 bitfield@ DFX_BS_SIZE_0_VALUE@
DFX_BS_SIZE_0_ADDR $ffffffff #0 bitfield! DFX_BS_SIZE_0_VALUE!

\ BS_ID_1 - Bitstream 1 ID.
DFX_BASE_ADDR $70 + constant DFX_BS_ID_1_ADDR

\ -- Address Mask Offset
DFX_BS_ID_1_ADDR $ffffffff #0 bitfield@ DFX_BS_ID_1_VALUE@
DFX_BS_ID_1_ADDR $ffffffff #0 bitfield! DFX_BS_ID_1_VALUE!

\ BS_ADDRESS_1 - Bitstream 1 byte address.
DFX_BASE_ADDR $74 + constant DFX_BS_ADDRESS_1_ADDR

\ -- Address Mask Offset
DFX_BS_ADDRESS_1_ADDR $ffffffff #0 bitfield@ DFX_BS_ADDRESS_1_VALUE@
DFX_BS_ADDRESS_1_ADDR $ffffffff #0 bitfield! DFX_BS_ADDRESS_1_VALUE!

\ BS_SIZE_1 - Bitstream 1 size in bytes.
DFX_BASE_ADDR $78 + constant DFX_BS_SIZE_1_ADDR

\ -- Address Mask Offset
DFX_BS_SIZE_1_ADDR $ffffffff #0 bitfield@ DFX_BS_SIZE_1_VALUE@
DFX_BS_SIZE_1_ADDR $ffffffff #0 bitfield! DFX_BS_SIZE_1_VALUE!
