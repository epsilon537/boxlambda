\ --- RESET

$100000d0 constant RESET_BASE_ADDR

\ CTRL - Reset control register
$0 constant RESET_CTRL_ADDR

\ CTRL.NDM_RESET - Non-debug module reset
1 constant RESET_CTRL_NDM_RESET_WIDTH
0 constant RESET_CTRL_NDM_RESET_LSB
$1 constant RESET_CTRL_NDM_RESET_MASK
\ CTRL.DM_RESET - Debug module reset
1 constant RESET_CTRL_DM_RESET_WIDTH
1 constant RESET_CTRL_DM_RESET_LSB
$2 constant RESET_CTRL_DM_RESET_MASK
\ CTRL.USB_RESET - USB module reset
1 constant RESET_CTRL_USB_RESET_WIDTH
2 constant RESET_CTRL_USB_RESET_LSB
$4 constant RESET_CTRL_USB_RESET_MASK
\ REASON - Reset reason register
$4 constant RESET_REASON_ADDR

\ REASON.POR - Power-on reset
1 constant RESET_REASON_POR_WIDTH
0 constant RESET_REASON_POR_LSB
$1 constant RESET_REASON_POR_MASK
\ REASON.SW_NDM - Software triggered NDM reset
1 constant RESET_REASON_SW_NDM_WIDTH
1 constant RESET_REASON_SW_NDM_LSB
$2 constant RESET_REASON_SW_NDM_MASK
\ REASON.SW_DM - Software triggered DM reset
1 constant RESET_REASON_SW_DM_WIDTH
2 constant RESET_REASON_SW_DM_LSB
$4 constant RESET_REASON_SW_DM_MASK
\ REASON.NDM - Non-debug module reset
1 constant RESET_REASON_NDM_WIDTH
3 constant RESET_REASON_NDM_LSB
$8 constant RESET_REASON_NDM_MASK
\ REASON.EXT - External reset
1 constant RESET_REASON_EXT_WIDTH
4 constant RESET_REASON_EXT_LSB
$10 constant RESET_REASON_EXT_MASK
\ REASON.SW_USB - Software triggered USB reset
1 constant RESET_REASON_SW_USB_WIDTH
5 constant RESET_REASON_SW_USB_LSB
$20 constant RESET_REASON_SW_USB_MASK
