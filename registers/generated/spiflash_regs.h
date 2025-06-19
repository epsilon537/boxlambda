// Created with Corsair v1.0.4
#ifndef __SPIFLASH_REGS_H
#define __SPIFLASH_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define SPIFLASH_BASE_ADDR 0x100000c0

// CTRL - SPIFlash Control Register
#define SPIFLASH_CTRL_ADDR 0x0
#define SPIFLASH_CTRL_RESET 0x0
typedef struct {
    uint32_t DATA : 8; // If control port is active, written byte value is sent out to SPI slave, top bit first. After write, may be read to retrieve return data byte.
    uint32_t CS_N : 1; // 1/0 de/activates the control port.
    uint32_t : 23; // reserved
} spiflash_ctrl_t;

// CTRL.DATA - If control port is active, written byte value is sent out to SPI slave, top bit first. After write, may be read to retrieve return data byte.
#define SPIFLASH_CTRL_DATA_WIDTH 8
#define SPIFLASH_CTRL_DATA_LSB 0
#define SPIFLASH_CTRL_DATA_MASK 0xff
#define SPIFLASH_CTRL_DATA_RESET 0x0

// CTRL.CS_N - 1/0 de/activates the control port.
#define SPIFLASH_CTRL_CS_N_WIDTH 1
#define SPIFLASH_CTRL_CS_N_LSB 8
#define SPIFLASH_CTRL_CS_N_MASK 0x100
#define SPIFLASH_CTRL_CS_N_RESET 0x0


// Register map structure
typedef struct {
    union {
        __IO uint32_t CTRL; // SPIFlash Control Register
        __IO spiflash_ctrl_t CTRL_bf; // Bit access for CTRL register
    };
} spiflash_t;

#define SPIFLASH ((spiflash_t*)(SPIFLASH_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __SPIFLASH_REGS_H */
