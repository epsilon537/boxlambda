\ --- DFX

$10000400 constant DFX_BASE_ADDR

\ STATUS - Status register.
$0 constant DFX_STATUS_ADDR

\ STATUS.STATE - Current state.
3 constant DFX_STATUS_STATE_WIDTH
0 constant DFX_STATUS_STATE_LSB
$7 constant DFX_STATUS_STATE_MASK
\ STATUS.ERR - Error state.
4 constant DFX_STATUS_ERR_WIDTH
3 constant DFX_STATUS_ERR_LSB
$78 constant DFX_STATUS_ERR_MASK
\ STATUS.SHUTDOWN - Set if controller is int shutdown state.
1 constant DFX_STATUS_SHUTDOWN_WIDTH
7 constant DFX_STATUS_SHUTDOWN_LSB
$80 constant DFX_STATUS_SHUTDOWN_MASK
\ STATUS.RM_ID - ID of Reconfigurable module to which the status applies.
16 constant DFX_STATUS_RM_ID_WIDTH
8 constant DFX_STATUS_RM_ID_LSB
$ffff00 constant DFX_STATUS_RM_ID_MASK
\ SW_TRIGGER - Software trigger register.
$4 constant DFX_SW_TRIGGER_ADDR

\ SW_TRIGGER.TRIGGER_ID - Trigger ID.
1 constant DFX_SW_TRIGGER_TRIGGER_ID_WIDTH
0 constant DFX_SW_TRIGGER_TRIGGER_ID_LSB
$1 constant DFX_SW_TRIGGER_TRIGGER_ID_MASK
\ SW_TRIGGER.TRIGGER_PENDING - Software trigger pending.
1 constant DFX_SW_TRIGGER_TRIGGER_PENDING_WIDTH
31 constant DFX_SW_TRIGGER_TRIGGER_PENDING_LSB
$80000000 constant DFX_SW_TRIGGER_TRIGGER_PENDING_MASK
\ TRIGGER_0 - Trigger 0.
$20 constant DFX_TRIGGER_0_ADDR

\ TRIGGER_0.VALUE - ID of Reconfigurable Module to load if trigger 0 is asserted.
32 constant DFX_TRIGGER_0_VALUE_WIDTH
0 constant DFX_TRIGGER_0_VALUE_LSB
$ffffffff constant DFX_TRIGGER_0_VALUE_MASK
\ TRIGGER_1 - Trigger 1.
$24 constant DFX_TRIGGER_1_ADDR

\ TRIGGER_1.VALUE - ID of Reconfigurable Module to load if trigger 1 is asserted.
32 constant DFX_TRIGGER_1_VALUE_WIDTH
0 constant DFX_TRIGGER_1_VALUE_LSB
$ffffffff constant DFX_TRIGGER_1_VALUE_MASK
\ RM_BS_INDEX_0 - Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 0.
$40 constant DFX_RM_BS_INDEX_0_ADDR

\ RM_BS_INDEX_0.INDEX - Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 0.
16 constant DFX_RM_BS_INDEX_0_INDEX_WIDTH
0 constant DFX_RM_BS_INDEX_0_INDEX_LSB
$ffff constant DFX_RM_BS_INDEX_0_INDEX_MASK
\ RM_CONTROL_0 - Control info for Reconfigurable Module 0.
$44 constant DFX_RM_CONTROL_0_ADDR

\ RM_CONTROL_0.SHUTDOWN_REQUIRED - Shutdown required.
2 constant DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_WIDTH
0 constant DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_LSB
$3 constant DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_MASK
\ RM_CONTROL_0.STARTUP_REQUIRED - Software startup required.
1 constant DFX_RM_CONTROL_0_STARTUP_REQUIRED_WIDTH
2 constant DFX_RM_CONTROL_0_STARTUP_REQUIRED_LSB
$4 constant DFX_RM_CONTROL_0_STARTUP_REQUIRED_MASK
\ RM_CONTROL_0.RST_REQUIRED - Reset required.
2 constant DFX_RM_CONTROL_0_RST_REQUIRED_WIDTH
3 constant DFX_RM_CONTROL_0_RST_REQUIRED_LSB
$18 constant DFX_RM_CONTROL_0_RST_REQUIRED_MASK
\ RM_CONTROL_0.RST_DURATION - Reset duration in clock cycles.
8 constant DFX_RM_CONTROL_0_RST_DURATION_WIDTH
5 constant DFX_RM_CONTROL_0_RST_DURATION_LSB
$1fe0 constant DFX_RM_CONTROL_0_RST_DURATION_MASK
\ RM_BS_INDEX_1 - Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 1.
$48 constant DFX_RM_BS_INDEX_1_ADDR

\ RM_BS_INDEX_1.INDEX - Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 1.
16 constant DFX_RM_BS_INDEX_1_INDEX_WIDTH
0 constant DFX_RM_BS_INDEX_1_INDEX_LSB
$ffff constant DFX_RM_BS_INDEX_1_INDEX_MASK
\ RM_CONTROL_1 - Control info for Reconfigurable Module 1.
$4c constant DFX_RM_CONTROL_1_ADDR

\ RM_CONTROL_1.SHUTDOWN_REQUIRED - Shutdown required.
2 constant DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_WIDTH
0 constant DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_LSB
$3 constant DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_MASK
\ RM_CONTROL_1.STARTUP_REQUIRED - Software startup required.
1 constant DFX_RM_CONTROL_1_STARTUP_REQUIRED_WIDTH
2 constant DFX_RM_CONTROL_1_STARTUP_REQUIRED_LSB
$4 constant DFX_RM_CONTROL_1_STARTUP_REQUIRED_MASK
\ RM_CONTROL_1.RST_REQUIRED - Reset required.
2 constant DFX_RM_CONTROL_1_RST_REQUIRED_WIDTH
3 constant DFX_RM_CONTROL_1_RST_REQUIRED_LSB
$18 constant DFX_RM_CONTROL_1_RST_REQUIRED_MASK
\ RM_CONTROL_1.RST_DURATION - Reset duration in clock cycles.
8 constant DFX_RM_CONTROL_1_RST_DURATION_WIDTH
5 constant DFX_RM_CONTROL_1_RST_DURATION_LSB
$1fe0 constant DFX_RM_CONTROL_1_RST_DURATION_MASK
\ BS_ID_0 - Bitstream 0 ID.
$60 constant DFX_BS_ID_0_ADDR

\ BS_ID_0.VALUE - Bitstream 0 ID value.
32 constant DFX_BS_ID_0_VALUE_WIDTH
0 constant DFX_BS_ID_0_VALUE_LSB
$ffffffff constant DFX_BS_ID_0_VALUE_MASK
\ BS_ADDRESS_0 - Bitstream 0 byte address.
$64 constant DFX_BS_ADDRESS_0_ADDR

\ BS_ADDRESS_0.VALUE - Bitstream 0 byte address.
32 constant DFX_BS_ADDRESS_0_VALUE_WIDTH
0 constant DFX_BS_ADDRESS_0_VALUE_LSB
$ffffffff constant DFX_BS_ADDRESS_0_VALUE_MASK
\ BS_SIZE_0 - Bitstream 0 size in bytes.
$68 constant DFX_BS_SIZE_0_ADDR

\ BS_SIZE_0.VALUE - Bitstream 0 size in bytes.
32 constant DFX_BS_SIZE_0_VALUE_WIDTH
0 constant DFX_BS_SIZE_0_VALUE_LSB
$ffffffff constant DFX_BS_SIZE_0_VALUE_MASK
\ BS_ID_1 - Bitstream 1 ID.
$70 constant DFX_BS_ID_1_ADDR

\ BS_ID_1.VALUE - Bitstream 1 ID value.
32 constant DFX_BS_ID_1_VALUE_WIDTH
0 constant DFX_BS_ID_1_VALUE_LSB
$ffffffff constant DFX_BS_ID_1_VALUE_MASK
\ BS_ADDRESS_1 - Bitstream 1 byte address.
$74 constant DFX_BS_ADDRESS_1_ADDR

\ BS_ADDRESS_1.VALUE - Bitstream 1 byte address value.
32 constant DFX_BS_ADDRESS_1_VALUE_WIDTH
0 constant DFX_BS_ADDRESS_1_VALUE_LSB
$ffffffff constant DFX_BS_ADDRESS_1_VALUE_MASK
\ BS_SIZE_1 - Bitstream 1 size in bytes.
$78 constant DFX_BS_SIZE_1_ADDR

\ BS_SIZE_1.VALUE - Bitstream 1 size in bytes value.
32 constant DFX_BS_SIZE_1_VALUE_WIDTH
0 constant DFX_BS_SIZE_1_VALUE_LSB
$ffffffff constant DFX_BS_SIZE_1_VALUE_MASK
