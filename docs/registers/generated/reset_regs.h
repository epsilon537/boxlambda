// Created with Corsair v1.0.4
#ifndef __RESET_REGS_H
#define __RESET_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define RESET_BASE_ADDR 0x100000d0

// CTRL - Reset control register
#define RESET_CTRL_ADDR 0x0
#define RESET_CTRL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t NDM_RESET : 1; // Non-debug module reset
    uint32_t DM_RESET : 1; // Debug module reset
    uint32_t USB_RESET : 1; // USB module reset
    uint32_t : 29; // reserved
  };
} reset_ctrl_t;

// CTRL.NDM_RESET - Non-debug module reset
#define RESET_CTRL_NDM_RESET_WIDTH 1
#define RESET_CTRL_NDM_RESET_LSB 0
#define RESET_CTRL_NDM_RESET_MASK 0x1
#define RESET_CTRL_NDM_RESET_RESET 0x0

// CTRL.DM_RESET - Debug module reset
#define RESET_CTRL_DM_RESET_WIDTH 1
#define RESET_CTRL_DM_RESET_LSB 1
#define RESET_CTRL_DM_RESET_MASK 0x2
#define RESET_CTRL_DM_RESET_RESET 0x0

// CTRL.USB_RESET - USB module reset
#define RESET_CTRL_USB_RESET_WIDTH 1
#define RESET_CTRL_USB_RESET_LSB 2
#define RESET_CTRL_USB_RESET_MASK 0x4
#define RESET_CTRL_USB_RESET_RESET 0x0

// REASON - Reset reason register
#define RESET_REASON_ADDR 0x4
#define RESET_REASON_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t POR : 1; // Power-on reset
    uint32_t SW_NDM : 1; // Software triggered NDM reset
    uint32_t SW_DM : 1; // Software triggered DM reset
    uint32_t NDM : 1; // Non-debug module reset
    uint32_t EXT : 1; // External reset
    uint32_t SW_USB : 1; // Software triggered USB reset
    uint32_t : 26; // reserved
  };
} reset_reason_t;

// REASON.POR - Power-on reset
#define RESET_REASON_POR_WIDTH 1
#define RESET_REASON_POR_LSB 0
#define RESET_REASON_POR_MASK 0x1
#define RESET_REASON_POR_RESET 0x0

// REASON.SW_NDM - Software triggered NDM reset
#define RESET_REASON_SW_NDM_WIDTH 1
#define RESET_REASON_SW_NDM_LSB 1
#define RESET_REASON_SW_NDM_MASK 0x2
#define RESET_REASON_SW_NDM_RESET 0x0

// REASON.SW_DM - Software triggered DM reset
#define RESET_REASON_SW_DM_WIDTH 1
#define RESET_REASON_SW_DM_LSB 2
#define RESET_REASON_SW_DM_MASK 0x4
#define RESET_REASON_SW_DM_RESET 0x0

// REASON.NDM - Non-debug module reset
#define RESET_REASON_NDM_WIDTH 1
#define RESET_REASON_NDM_LSB 3
#define RESET_REASON_NDM_MASK 0x8
#define RESET_REASON_NDM_RESET 0x0

// REASON.EXT - External reset
#define RESET_REASON_EXT_WIDTH 1
#define RESET_REASON_EXT_LSB 4
#define RESET_REASON_EXT_MASK 0x10
#define RESET_REASON_EXT_RESET 0x0

// REASON.SW_USB - Software triggered USB reset
#define RESET_REASON_SW_USB_WIDTH 1
#define RESET_REASON_SW_USB_LSB 5
#define RESET_REASON_SW_USB_MASK 0x20
#define RESET_REASON_SW_USB_RESET 0x0


// Register map structure
typedef struct {
    union {
        __O uint32_t CTRL; // Reset control register
        __O reset_ctrl_t CTRL_bf; // Bit access for CTRL register
    };
    union {
        __I uint32_t REASON; // Reset reason register
        __I reset_reason_t REASON_bf; // Bit access for REASON register
    };
} reset_t;

#define RESET ((reset_t*)(RESET_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __RESET_REGS_H */
