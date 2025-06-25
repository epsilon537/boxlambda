// Created with Corsair v1.0.4
#ifndef __DFX_CONTROLLER_REGS_H
#define __DFX_CONTROLLER_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define DFX_BASE_ADDR 0x10000400

// STATUS - Status register.
#define DFX_STATUS_ADDR 0x0
#define DFX_STATUS_RESET 0x0
typedef struct {
    uint32_t STATE : 3; // Current state.
    uint32_t ERR : 4; // Error state.
    uint32_t SHUTDOWN : 1; // Set if controller is int shutdown state.
    uint32_t RM_ID : 16; // ID of Reconfigurable module to which the status applies.
    uint32_t : 8; // reserved
} dfx_status_t;

// STATUS.STATE - Current state.
#define DFX_STATUS_STATE_WIDTH 3
#define DFX_STATUS_STATE_LSB 0
#define DFX_STATUS_STATE_MASK 0x7
#define DFX_STATUS_STATE_RESET 0x0
typedef enum {
    DFX_STATUS_STATE_VS_EMPTY = 0x0, //Virtual socket empty.
    DFX_STATUS_STATE_HW_SHUTDOWN = 0x1, //Hardware shutdown step.
    DFX_STATUS_STATE_SW_SHUTDOWN = 0x2, //Executing software shutdown step.
    DFX_STATUS_STATE_LOADING = 0x4, //Loading the new reconfigurable module.
    DFX_STATUS_STATE_SW_STARTUP = 0x5, //Executing software startup step.
    DFX_STATUS_STATE_RM_RESET = 0x6, //Executing reconfigurable module reset step.
    DFX_STATUS_STATE_VS_FULL = 0x7, //Virtual Socket is full.
} dfx_status_state_t;

// STATUS.ERR - Error state.
#define DFX_STATUS_ERR_WIDTH 4
#define DFX_STATUS_ERR_LSB 3
#define DFX_STATUS_ERR_MASK 0x78
#define DFX_STATUS_ERR_RESET 0x0
typedef enum {
    DFX_STATUS_ERR_NO_ERR = 0x0, //No error.
    DFX_STATUS_ERR_BAD_CONFIG = 0x1, //Bad configuration error.
    DFX_STATUS_ERR_BS = 0x2, //Bitstream error.
    DFX_STATUS_ERR_FETCH = 0x4, //Fetch error.
    DFX_STATUS_ERR_BS_FETCH = 0x5, //Bitstream and fetch error.
    DFX_STATUS_ERR_BAD_SIZE = 0x7, //Bad size error.
    DFX_STATUS_ERR_BAD_FORMAT = 0x8, //Bad format error.
    DFX_STATUS_ERR_UNKNOWN = 0xf, //Unknown error.
} dfx_status_err_t;

// STATUS.SHUTDOWN - Set if controller is int shutdown state.
#define DFX_STATUS_SHUTDOWN_WIDTH 1
#define DFX_STATUS_SHUTDOWN_LSB 7
#define DFX_STATUS_SHUTDOWN_MASK 0x80
#define DFX_STATUS_SHUTDOWN_RESET 0x0

// STATUS.RM_ID - ID of Reconfigurable module to which the status applies.
#define DFX_STATUS_RM_ID_WIDTH 16
#define DFX_STATUS_RM_ID_LSB 8
#define DFX_STATUS_RM_ID_MASK 0xffff00
#define DFX_STATUS_RM_ID_RESET 0x0

// SW_TRIGGER - Software trigger register.
#define DFX_SW_TRIGGER_ADDR 0x4
#define DFX_SW_TRIGGER_RESET 0x0
typedef struct {
    uint32_t TRIGGER_ID : 1; // Trigger ID.
    uint32_t : 30; // reserved
    uint32_t TRIGGER_PENDING : 1; // Software trigger pending.
} dfx_sw_trigger_t;

// SW_TRIGGER.TRIGGER_ID - Trigger ID.
#define DFX_SW_TRIGGER_TRIGGER_ID_WIDTH 1
#define DFX_SW_TRIGGER_TRIGGER_ID_LSB 0
#define DFX_SW_TRIGGER_TRIGGER_ID_MASK 0x1
#define DFX_SW_TRIGGER_TRIGGER_ID_RESET 0x0

// SW_TRIGGER.TRIGGER_PENDING - Software trigger pending.
#define DFX_SW_TRIGGER_TRIGGER_PENDING_WIDTH 1
#define DFX_SW_TRIGGER_TRIGGER_PENDING_LSB 31
#define DFX_SW_TRIGGER_TRIGGER_PENDING_MASK 0x80000000
#define DFX_SW_TRIGGER_TRIGGER_PENDING_RESET 0x0

// TRIGGER_0 - Trigger 0.
#define DFX_TRIGGER_0_ADDR 0x20
#define DFX_TRIGGER_0_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // ID of Reconfigurable Module to load if trigger 0 is asserted.
} dfx_trigger_0_t;

// TRIGGER_0.VALUE - ID of Reconfigurable Module to load if trigger 0 is asserted.
#define DFX_TRIGGER_0_VALUE_WIDTH 32
#define DFX_TRIGGER_0_VALUE_LSB 0
#define DFX_TRIGGER_0_VALUE_MASK 0xffffffff
#define DFX_TRIGGER_0_VALUE_RESET 0x0

// TRIGGER_1 - Trigger 1.
#define DFX_TRIGGER_1_ADDR 0x24
#define DFX_TRIGGER_1_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // ID of Reconfigurable Module to load if trigger 1 is asserted.
} dfx_trigger_1_t;

// TRIGGER_1.VALUE - ID of Reconfigurable Module to load if trigger 1 is asserted.
#define DFX_TRIGGER_1_VALUE_WIDTH 32
#define DFX_TRIGGER_1_VALUE_LSB 0
#define DFX_TRIGGER_1_VALUE_MASK 0xffffffff
#define DFX_TRIGGER_1_VALUE_RESET 0x0

// RM_BS_INDEX_0 - Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 0.
#define DFX_RM_BS_INDEX_0_ADDR 0x40
#define DFX_RM_BS_INDEX_0_RESET 0x0
typedef struct {
    uint32_t INDEX : 16; // Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 0.
    uint32_t : 16; // reserved
} dfx_rm_bs_index_0_t;

// RM_BS_INDEX_0.INDEX - Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 0.
#define DFX_RM_BS_INDEX_0_INDEX_WIDTH 16
#define DFX_RM_BS_INDEX_0_INDEX_LSB 0
#define DFX_RM_BS_INDEX_0_INDEX_MASK 0xffff
#define DFX_RM_BS_INDEX_0_INDEX_RESET 0x0

// RM_CONTROL_0 - Control info for Reconfigurable Module 0.
#define DFX_RM_CONTROL_0_ADDR 0x44
#define DFX_RM_CONTROL_0_RESET 0x0
typedef struct {
    uint32_t SHUTDOWN_REQUIRED : 2; // Shutdown required.
    uint32_t STARTUP_REQUIRED : 1; // Software startup required.
    uint32_t RST_REQUIRED : 2; // Reset required.
    uint32_t RST_DURATION : 8; // Reset duration in clock cycles.
    uint32_t : 19; // reserved
} dfx_rm_control_0_t;

// RM_CONTROL_0.SHUTDOWN_REQUIRED - Shutdown required.
#define DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_WIDTH 2
#define DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_LSB 0
#define DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_MASK 0x3
#define DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_RESET 0x0
typedef enum {
    DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_NO_SHUTDOWN = 0x0, //No shutdown required.
    DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_HW_RM = 0x1, //Hardware Reconfigurable Module shutdown required.
    DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_HW_THEN_SW = 0x2, //Hardware then software shutdown required.
    DFX_RM_CONTROL_0_SHUTDOWN_REQUIRED_SW_THEN_HW = 0x3, //Software then hardware shutdown required.
} dfx_rm_control_0_shutdown_required_t;

// RM_CONTROL_0.STARTUP_REQUIRED - Software startup required.
#define DFX_RM_CONTROL_0_STARTUP_REQUIRED_WIDTH 1
#define DFX_RM_CONTROL_0_STARTUP_REQUIRED_LSB 2
#define DFX_RM_CONTROL_0_STARTUP_REQUIRED_MASK 0x4
#define DFX_RM_CONTROL_0_STARTUP_REQUIRED_RESET 0x0

// RM_CONTROL_0.RST_REQUIRED - Reset required.
#define DFX_RM_CONTROL_0_RST_REQUIRED_WIDTH 2
#define DFX_RM_CONTROL_0_RST_REQUIRED_LSB 3
#define DFX_RM_CONTROL_0_RST_REQUIRED_MASK 0x18
#define DFX_RM_CONTROL_0_RST_REQUIRED_RESET 0x0
typedef enum {
    DFX_RM_CONTROL_0_RST_REQUIRED_NO_RST = 0x0, //No reset required.
    DFX_RM_CONTROL_0_RST_REQUIRED_ACTIVE_LO_RST = 0x2, //Active low reset required.
    DFX_RM_CONTROL_0_RST_REQUIRED_ACTIVE_HI_RST = 0x3, //Active high reset required.
} dfx_rm_control_0_rst_required_t;

// RM_CONTROL_0.RST_DURATION - Reset duration in clock cycles.
#define DFX_RM_CONTROL_0_RST_DURATION_WIDTH 8
#define DFX_RM_CONTROL_0_RST_DURATION_LSB 5
#define DFX_RM_CONTROL_0_RST_DURATION_MASK 0x1fe0
#define DFX_RM_CONTROL_0_RST_DURATION_RESET 0x0

// RM_BS_INDEX_1 - Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 1.
#define DFX_RM_BS_INDEX_1_ADDR 0x48
#define DFX_RM_BS_INDEX_1_RESET 0x0
typedef struct {
    uint32_t INDEX : 16; // Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 1.
    uint32_t : 16; // reserved
} dfx_rm_bs_index_1_t;

// RM_BS_INDEX_1.INDEX - Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 1.
#define DFX_RM_BS_INDEX_1_INDEX_WIDTH 16
#define DFX_RM_BS_INDEX_1_INDEX_LSB 0
#define DFX_RM_BS_INDEX_1_INDEX_MASK 0xffff
#define DFX_RM_BS_INDEX_1_INDEX_RESET 0x0

// RM_CONTROL_1 - Control info for Reconfigurable Module 1.
#define DFX_RM_CONTROL_1_ADDR 0x4c
#define DFX_RM_CONTROL_1_RESET 0x0
typedef struct {
    uint32_t SHUTDOWN_REQUIRED : 2; // Shutdown required.
    uint32_t STARTUP_REQUIRED : 1; // Software startup required.
    uint32_t RST_REQUIRED : 2; // Reset required.
    uint32_t RST_DURATION : 8; // Reset duration in clock cycles.
    uint32_t : 19; // reserved
} dfx_rm_control_1_t;

// RM_CONTROL_1.SHUTDOWN_REQUIRED - Shutdown required.
#define DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_WIDTH 2
#define DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_LSB 0
#define DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_MASK 0x3
#define DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_RESET 0x0
typedef enum {
    DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_NO_SHUTDOWN = 0x0, //No shutdown required.
    DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_HW_RM = 0x1, //Hardware Reconfigurable Module shutdown required.
    DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_HW_THEN_SW = 0x2, //Hardware then software shutdown required.
    DFX_RM_CONTROL_1_SHUTDOWN_REQUIRED_SW_THEN_HW = 0x3, //Software then hardware shutdown required.
} dfx_rm_control_1_shutdown_required_t;

// RM_CONTROL_1.STARTUP_REQUIRED - Software startup required.
#define DFX_RM_CONTROL_1_STARTUP_REQUIRED_WIDTH 1
#define DFX_RM_CONTROL_1_STARTUP_REQUIRED_LSB 2
#define DFX_RM_CONTROL_1_STARTUP_REQUIRED_MASK 0x4
#define DFX_RM_CONTROL_1_STARTUP_REQUIRED_RESET 0x0

// RM_CONTROL_1.RST_REQUIRED - Reset required.
#define DFX_RM_CONTROL_1_RST_REQUIRED_WIDTH 2
#define DFX_RM_CONTROL_1_RST_REQUIRED_LSB 3
#define DFX_RM_CONTROL_1_RST_REQUIRED_MASK 0x18
#define DFX_RM_CONTROL_1_RST_REQUIRED_RESET 0x0
typedef enum {
    DFX_RM_CONTROL_1_RST_REQUIRED_NO_RST = 0x0, //No reset required.
    DFX_RM_CONTROL_1_RST_REQUIRED_ACTIVE_LO_RST = 0x2, //Active low reset required.
    DFX_RM_CONTROL_1_RST_REQUIRED_ACTIVE_HI_RST = 0x3, //Active high reset required.
} dfx_rm_control_1_rst_required_t;

// RM_CONTROL_1.RST_DURATION - Reset duration in clock cycles.
#define DFX_RM_CONTROL_1_RST_DURATION_WIDTH 8
#define DFX_RM_CONTROL_1_RST_DURATION_LSB 5
#define DFX_RM_CONTROL_1_RST_DURATION_MASK 0x1fe0
#define DFX_RM_CONTROL_1_RST_DURATION_RESET 0x0

// BS_ID_0 - Bitstream 0 ID.
#define DFX_BS_ID_0_ADDR 0x60
#define DFX_BS_ID_0_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // Bitstream 0 ID value.
} dfx_bs_id_0_t;

// BS_ID_0.VALUE - Bitstream 0 ID value.
#define DFX_BS_ID_0_VALUE_WIDTH 32
#define DFX_BS_ID_0_VALUE_LSB 0
#define DFX_BS_ID_0_VALUE_MASK 0xffffffff
#define DFX_BS_ID_0_VALUE_RESET 0x0

// BS_ADDRESS_0 - Bitstream 0 byte address.
#define DFX_BS_ADDRESS_0_ADDR 0x64
#define DFX_BS_ADDRESS_0_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // Bitstream 0 byte address.
} dfx_bs_address_0_t;

// BS_ADDRESS_0.VALUE - Bitstream 0 byte address.
#define DFX_BS_ADDRESS_0_VALUE_WIDTH 32
#define DFX_BS_ADDRESS_0_VALUE_LSB 0
#define DFX_BS_ADDRESS_0_VALUE_MASK 0xffffffff
#define DFX_BS_ADDRESS_0_VALUE_RESET 0x0

// BS_SIZE_0 - Bitstream 0 size in bytes.
#define DFX_BS_SIZE_0_ADDR 0x68
#define DFX_BS_SIZE_0_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // Bitstream 0 size in bytes.
} dfx_bs_size_0_t;

// BS_SIZE_0.VALUE - Bitstream 0 size in bytes.
#define DFX_BS_SIZE_0_VALUE_WIDTH 32
#define DFX_BS_SIZE_0_VALUE_LSB 0
#define DFX_BS_SIZE_0_VALUE_MASK 0xffffffff
#define DFX_BS_SIZE_0_VALUE_RESET 0x0

// BS_ID_1 - Bitstream 1 ID.
#define DFX_BS_ID_1_ADDR 0x70
#define DFX_BS_ID_1_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // Bitstream 1 ID value.
} dfx_bs_id_1_t;

// BS_ID_1.VALUE - Bitstream 1 ID value.
#define DFX_BS_ID_1_VALUE_WIDTH 32
#define DFX_BS_ID_1_VALUE_LSB 0
#define DFX_BS_ID_1_VALUE_MASK 0xffffffff
#define DFX_BS_ID_1_VALUE_RESET 0x0

// BS_ADDRESS_1 - Bitstream 1 byte address.
#define DFX_BS_ADDRESS_1_ADDR 0x74
#define DFX_BS_ADDRESS_1_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // Bitstream 1 byte address value.
} dfx_bs_address_1_t;

// BS_ADDRESS_1.VALUE - Bitstream 1 byte address value.
#define DFX_BS_ADDRESS_1_VALUE_WIDTH 32
#define DFX_BS_ADDRESS_1_VALUE_LSB 0
#define DFX_BS_ADDRESS_1_VALUE_MASK 0xffffffff
#define DFX_BS_ADDRESS_1_VALUE_RESET 0x0

// BS_SIZE_1 - Bitstream 1 size in bytes.
#define DFX_BS_SIZE_1_ADDR 0x78
#define DFX_BS_SIZE_1_RESET 0x0
typedef struct {
    uint32_t VALUE : 32; // Bitstream 1 size in bytes value.
} dfx_bs_size_1_t;

// BS_SIZE_1.VALUE - Bitstream 1 size in bytes value.
#define DFX_BS_SIZE_1_VALUE_WIDTH 32
#define DFX_BS_SIZE_1_VALUE_LSB 0
#define DFX_BS_SIZE_1_VALUE_MASK 0xffffffff
#define DFX_BS_SIZE_1_VALUE_RESET 0x0


// Register map structure
typedef struct {
    union {
        __I uint32_t STATUS; // Status register.
        __I dfx_status_t STATUS_bf; // Bit access for STATUS register
    };
    union {
        __IO uint32_t SW_TRIGGER; // Software trigger register.
        __IO dfx_sw_trigger_t SW_TRIGGER_bf; // Bit access for SW_TRIGGER register
    };
    __IO uint32_t RESERVED0[6];
    union {
        __IO uint32_t TRIGGER_0; // Trigger 0.
        __IO dfx_trigger_0_t TRIGGER_0_bf; // Bit access for TRIGGER_0 register
    };
    union {
        __IO uint32_t TRIGGER_1; // Trigger 1.
        __IO dfx_trigger_1_t TRIGGER_1_bf; // Bit access for TRIGGER_1 register
    };
    __IO uint32_t RESERVED1[6];
    union {
        __IO uint32_t RM_BS_INDEX_0; // Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 0.
        __IO dfx_rm_bs_index_0_t RM_BS_INDEX_0_bf; // Bit access for RM_BS_INDEX_0 register
    };
    union {
        __IO uint32_t RM_CONTROL_0; // Control info for Reconfigurable Module 0.
        __IO dfx_rm_control_0_t RM_CONTROL_0_bf; // Bit access for RM_CONTROL_0 register
    };
    union {
        __IO uint32_t RM_BS_INDEX_1; // Row number in BS info register bank that holds info about the bitstream for Reconfigurable Module 1.
        __IO dfx_rm_bs_index_1_t RM_BS_INDEX_1_bf; // Bit access for RM_BS_INDEX_1 register
    };
    union {
        __IO uint32_t RM_CONTROL_1; // Control info for Reconfigurable Module 1.
        __IO dfx_rm_control_1_t RM_CONTROL_1_bf; // Bit access for RM_CONTROL_1 register
    };
    __IO uint32_t RESERVED2[4];
    union {
        __IO uint32_t BS_ID_0; // Bitstream 0 ID.
        __IO dfx_bs_id_0_t BS_ID_0_bf; // Bit access for BS_ID_0 register
    };
    union {
        __IO uint32_t BS_ADDRESS_0; // Bitstream 0 byte address.
        __IO dfx_bs_address_0_t BS_ADDRESS_0_bf; // Bit access for BS_ADDRESS_0 register
    };
    union {
        __IO uint32_t BS_SIZE_0; // Bitstream 0 size in bytes.
        __IO dfx_bs_size_0_t BS_SIZE_0_bf; // Bit access for BS_SIZE_0 register
    };
    __IO uint32_t RESERVED3[1];
    union {
        __IO uint32_t BS_ID_1; // Bitstream 1 ID.
        __IO dfx_bs_id_1_t BS_ID_1_bf; // Bit access for BS_ID_1 register
    };
    union {
        __IO uint32_t BS_ADDRESS_1; // Bitstream 1 byte address.
        __IO dfx_bs_address_1_t BS_ADDRESS_1_bf; // Bit access for BS_ADDRESS_1 register
    };
    union {
        __IO uint32_t BS_SIZE_1; // Bitstream 1 size in bytes.
        __IO dfx_bs_size_1_t BS_SIZE_1_bf; // Bit access for BS_SIZE_1 register
    };
} dfx_t;

#define DFX ((dfx_t*)(DFX_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __DFX_CONTROLLER_REGS_H */
