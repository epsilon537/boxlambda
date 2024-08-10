#ifndef RESET_REGS_H
#define RESET_REGS_H

//Reset Controller Registers

//Reset Control Register
#define RESET_CTRL 0x100000D0
#define RESET_CTRL_NDM_RST 0x00000001 /*Trigger a Non-Debug Module Reset*/
#define RESET_CTRL_DM_RST  0x00000002 /*Trigger a Debug Module Reset*/
#define RESET_CTRL_USB_RST 0x00000004 /*Trigger a USB Reset*/

//Reset Reason Register
#define RESET_REASON 0x100000D4
#define RESET_REASON_POR 0x00000001    /*Power-On Reset*/
#define RESET_REASON_SW_NDM 0x00000002 /*SW triggered Non-Debug Module Reset*/
#define RESET_REASON_SW_DM  0x00000004 /*SW triggered Debug Module Reset*/
#define RESET_REASON_NDM    0x00000008 /*Non-Debug Module Reset*/
#define RESET_REASON_EXT    0x00000010 /*External Reset*/
#define RESET_REASON_SW_USB 0x00000020 /*SW triggered USB reset*/

inline void reset_ctrl_wr(uint32_t data) {
  *(volatile uint32_t *)(RESET_CTRL) = data;
}

inline uint32_t reset_reason_rd_rst() {
  uint32_t res = *(uint32_t volatile *)(RESET_REASON);
  return res;
}

#endif /*RESET_REGS_H*/

