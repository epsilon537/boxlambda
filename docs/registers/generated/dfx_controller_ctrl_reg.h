// Created with Corsair v1.0.4
#ifndef __DFX_CONTROLLER_CTRL_REG_H
#define __DFX_CONTROLLER_CTRL_REG_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define DFX_CTRL_BASE_ADDR 0x10000400

// CONTROL - Control register.
#define DFX_CTRL_CONTROL_ADDR 0x0
#define DFX_CTRL_CONTROL_RESET 0x0
typedef struct {
    uint32_t CMD : 8; // Command.
    uint32_t BYTE : 8; // Byte field containing extra info.
    uint32_t HALFWORD : 16; // Halfword field containing extra info.
} dfx_ctrl_control_t;

// CONTROL.CMD - Command.
#define DFX_CTRL_CONTROL_CMD_WIDTH 8
#define DFX_CTRL_CONTROL_CMD_LSB 0
#define DFX_CTRL_CONTROL_CMD_MASK 0xff
#define DFX_CTRL_CONTROL_CMD_RESET 0x0
typedef enum {
    DFX_CTRL_CONTROL_CMD_SHUTDOWN = 0x0, //Shutdown.
    DFX_CTRL_CONTROL_CMD_RESTART_NO_STAT = 0x1, //Restart without status.
    DFX_CTRL_CONTROL_CMD_RESTART_STAT = 0x2, //Restart with status.
    DFX_CTRL_CONTROL_CMD_PROCEED = 0x3, //Proceed.
    DFX_CTRL_CONTROL_CMD_USR_CTRL = 0x4, //Proceed.
} dfx_ctrl_control_cmd_t;

// CONTROL.BYTE - Byte field containing extra info.
#define DFX_CTRL_CONTROL_BYTE_WIDTH 8
#define DFX_CTRL_CONTROL_BYTE_LSB 8
#define DFX_CTRL_CONTROL_BYTE_MASK 0xff00
#define DFX_CTRL_CONTROL_BYTE_RESET 0x0

// CONTROL.HALFWORD - Halfword field containing extra info.
#define DFX_CTRL_CONTROL_HALFWORD_WIDTH 16
#define DFX_CTRL_CONTROL_HALFWORD_LSB 16
#define DFX_CTRL_CONTROL_HALFWORD_MASK 0xffff0000
#define DFX_CTRL_CONTROL_HALFWORD_RESET 0x0


// Register map structure
typedef struct {
    union {
        __IO uint32_t CONTROL; // Control register.
        __IO dfx_ctrl_control_t CONTROL_bf; // Bit access for CONTROL register
    };
} dfx_ctrl_t;

#define DFX_CTRL ((dfx_ctrl_t*)(DFX_CTRL_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __DFX_CONTROLLER_CTRL_REG_H */
