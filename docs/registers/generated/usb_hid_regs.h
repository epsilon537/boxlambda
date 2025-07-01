// Created with Corsair v1.0.4
#ifndef __USB_HID_REGS_H
#define __USB_HID_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define USB_HID_BASE_ADDR 0x0

// IEN - Interrupt Enable Register
#define USB_HID_IEN_ADDR 0x0
#define USB_HID_IEN_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t USB_REPORT : 1; // Set to enable USB_REPORT interrupt.
    uint32_t LED_REPORT : 1; // Set to enable LED_REPORT interrupt.
    uint32_t : 30; // reserved
  };
} usb_hid_ien_t;

// IEN.USB_REPORT - Set to enable USB_REPORT interrupt.
#define USB_HID_IEN_USB_REPORT_WIDTH 1
#define USB_HID_IEN_USB_REPORT_LSB 0
#define USB_HID_IEN_USB_REPORT_MASK 0x1
#define USB_HID_IEN_USB_REPORT_RESET 0x0

// IEN.LED_REPORT - Set to enable LED_REPORT interrupt.
#define USB_HID_IEN_LED_REPORT_WIDTH 1
#define USB_HID_IEN_LED_REPORT_LSB 1
#define USB_HID_IEN_LED_REPORT_MASK 0x2
#define USB_HID_IEN_LED_REPORT_RESET 0x0

// ISR - Interrupt Status Register
#define USB_HID_ISR_ADDR 0x4
#define USB_HID_ISR_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t USB_REPORT : 1; // Set when a USB report is received.
    uint32_t LED_REPORT : 1; // Set when a LED report is received.
    uint32_t : 30; // reserved
  };
} usb_hid_isr_t;

// ISR.USB_REPORT - Set when a USB report is received.
#define USB_HID_ISR_USB_REPORT_WIDTH 1
#define USB_HID_ISR_USB_REPORT_LSB 0
#define USB_HID_ISR_USB_REPORT_MASK 0x1
#define USB_HID_ISR_USB_REPORT_RESET 0x0

// ISR.LED_REPORT - Set when a LED report is received.
#define USB_HID_ISR_LED_REPORT_WIDTH 1
#define USB_HID_ISR_LED_REPORT_LSB 1
#define USB_HID_ISR_LED_REPORT_MASK 0x2
#define USB_HID_ISR_LED_REPORT_RESET 0x0

// STATUS - Status Register
#define USB_HID_STATUS_ADDR 0x8
#define USB_HID_STATUS_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t USB_TYP : 2; // USB type
    uint32_t CONN_ERR : 1; // Connection error.
    uint32_t : 29; // reserved
  };
} usb_hid_status_t;

// STATUS.USB_TYP - USB type
#define USB_HID_STATUS_USB_TYP_WIDTH 2
#define USB_HID_STATUS_USB_TYP_LSB 0
#define USB_HID_STATUS_USB_TYP_MASK 0x3
#define USB_HID_STATUS_USB_TYP_RESET 0x0
typedef enum {
    USB_HID_STATUS_USB_TYP_NO_DEV = 0x0, //No device
    USB_HID_STATUS_USB_TYP_KEYB = 0x1, //Keyboard
    USB_HID_STATUS_USB_TYP_MOUSE = 0x2, //Mouse
    USB_HID_STATUS_USB_TYP_GAME = 0x3, //Game pad
} usb_hid_status_usb_typ_t;

// STATUS.CONN_ERR - Connection error.
#define USB_HID_STATUS_CONN_ERR_WIDTH 1
#define USB_HID_STATUS_CONN_ERR_LSB 2
#define USB_HID_STATUS_CONN_ERR_MASK 0x4
#define USB_HID_STATUS_CONN_ERR_RESET 0x0

// KEY_MODS - Key modifiers
#define USB_HID_KEY_MODS_ADDR 0xc
#define USB_HID_KEY_MODS_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t LCTRL : 1; // Left control
    uint32_t LSHIFT : 1; // Left shift
    uint32_t LALT : 1; // Left alt
    uint32_t LMETA : 1; // Left meta
    uint32_t RCTRL : 1; // Right control
    uint32_t RSHIFT : 1; // Right shift
    uint32_t RALT : 1; // Right alt
    uint32_t RMETA : 1; // Right meta
    uint32_t : 24; // reserved
  };
} usb_hid_key_mods_t;

// KEY_MODS.LCTRL - Left control
#define USB_HID_KEY_MODS_LCTRL_WIDTH 1
#define USB_HID_KEY_MODS_LCTRL_LSB 0
#define USB_HID_KEY_MODS_LCTRL_MASK 0x1
#define USB_HID_KEY_MODS_LCTRL_RESET 0x0

// KEY_MODS.LSHIFT - Left shift
#define USB_HID_KEY_MODS_LSHIFT_WIDTH 1
#define USB_HID_KEY_MODS_LSHIFT_LSB 1
#define USB_HID_KEY_MODS_LSHIFT_MASK 0x2
#define USB_HID_KEY_MODS_LSHIFT_RESET 0x0

// KEY_MODS.LALT - Left alt
#define USB_HID_KEY_MODS_LALT_WIDTH 1
#define USB_HID_KEY_MODS_LALT_LSB 2
#define USB_HID_KEY_MODS_LALT_MASK 0x4
#define USB_HID_KEY_MODS_LALT_RESET 0x0

// KEY_MODS.LMETA - Left meta
#define USB_HID_KEY_MODS_LMETA_WIDTH 1
#define USB_HID_KEY_MODS_LMETA_LSB 3
#define USB_HID_KEY_MODS_LMETA_MASK 0x8
#define USB_HID_KEY_MODS_LMETA_RESET 0x0

// KEY_MODS.RCTRL - Right control
#define USB_HID_KEY_MODS_RCTRL_WIDTH 1
#define USB_HID_KEY_MODS_RCTRL_LSB 4
#define USB_HID_KEY_MODS_RCTRL_MASK 0x10
#define USB_HID_KEY_MODS_RCTRL_RESET 0x0

// KEY_MODS.RSHIFT - Right shift
#define USB_HID_KEY_MODS_RSHIFT_WIDTH 1
#define USB_HID_KEY_MODS_RSHIFT_LSB 5
#define USB_HID_KEY_MODS_RSHIFT_MASK 0x20
#define USB_HID_KEY_MODS_RSHIFT_RESET 0x0

// KEY_MODS.RALT - Right alt
#define USB_HID_KEY_MODS_RALT_WIDTH 1
#define USB_HID_KEY_MODS_RALT_LSB 6
#define USB_HID_KEY_MODS_RALT_MASK 0x40
#define USB_HID_KEY_MODS_RALT_RESET 0x0

// KEY_MODS.RMETA - Right meta
#define USB_HID_KEY_MODS_RMETA_WIDTH 1
#define USB_HID_KEY_MODS_RMETA_LSB 7
#define USB_HID_KEY_MODS_RMETA_MASK 0x80
#define USB_HID_KEY_MODS_RMETA_RESET 0x0

// KEYS - Keys register
#define USB_HID_KEYS_ADDR 0x10
#define USB_HID_KEYS_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t KEY_0 : 8; // Key 0
    uint32_t KEY_1 : 8; // Key 1
    uint32_t KEY_2 : 8; // Key 2
    uint32_t KEY_3 : 8; // Key 3
  };
} usb_hid_keys_t;

// KEYS.KEY_0 - Key 0
#define USB_HID_KEYS_KEY_0_WIDTH 8
#define USB_HID_KEYS_KEY_0_LSB 0
#define USB_HID_KEYS_KEY_0_MASK 0xff
#define USB_HID_KEYS_KEY_0_RESET 0x0

// KEYS.KEY_1 - Key 1
#define USB_HID_KEYS_KEY_1_WIDTH 8
#define USB_HID_KEYS_KEY_1_LSB 8
#define USB_HID_KEYS_KEY_1_MASK 0xff00
#define USB_HID_KEYS_KEY_1_RESET 0x0

// KEYS.KEY_2 - Key 2
#define USB_HID_KEYS_KEY_2_WIDTH 8
#define USB_HID_KEYS_KEY_2_LSB 16
#define USB_HID_KEYS_KEY_2_MASK 0xff0000
#define USB_HID_KEYS_KEY_2_RESET 0x0

// KEYS.KEY_3 - Key 3
#define USB_HID_KEYS_KEY_3_WIDTH 8
#define USB_HID_KEYS_KEY_3_LSB 24
#define USB_HID_KEYS_KEY_3_MASK 0xff000000
#define USB_HID_KEYS_KEY_3_RESET 0x0

// MOUSE - Mouse register
#define USB_HID_MOUSE_ADDR 0x14
#define USB_HID_MOUSE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t DY : 8; // Delta y
    uint32_t DX : 8; // Delta x
    uint32_t BTN_LEFT : 1; // Left button
    uint32_t BTN_RIGHT : 1; // Right button
    uint32_t BTN_MIDDLE : 1; // Middle button
    uint32_t : 13; // reserved
  };
} usb_hid_mouse_t;

// MOUSE.DY - Delta y
#define USB_HID_MOUSE_DY_WIDTH 8
#define USB_HID_MOUSE_DY_LSB 0
#define USB_HID_MOUSE_DY_MASK 0xff
#define USB_HID_MOUSE_DY_RESET 0x0

// MOUSE.DX - Delta x
#define USB_HID_MOUSE_DX_WIDTH 8
#define USB_HID_MOUSE_DX_LSB 8
#define USB_HID_MOUSE_DX_MASK 0xff00
#define USB_HID_MOUSE_DX_RESET 0x0

// MOUSE.BTN_LEFT - Left button
#define USB_HID_MOUSE_BTN_LEFT_WIDTH 1
#define USB_HID_MOUSE_BTN_LEFT_LSB 16
#define USB_HID_MOUSE_BTN_LEFT_MASK 0x10000
#define USB_HID_MOUSE_BTN_LEFT_RESET 0x0

// MOUSE.BTN_RIGHT - Right button
#define USB_HID_MOUSE_BTN_RIGHT_WIDTH 1
#define USB_HID_MOUSE_BTN_RIGHT_LSB 17
#define USB_HID_MOUSE_BTN_RIGHT_MASK 0x20000
#define USB_HID_MOUSE_BTN_RIGHT_RESET 0x0

// MOUSE.BTN_MIDDLE - Middle button
#define USB_HID_MOUSE_BTN_MIDDLE_WIDTH 1
#define USB_HID_MOUSE_BTN_MIDDLE_LSB 18
#define USB_HID_MOUSE_BTN_MIDDLE_MASK 0x40000
#define USB_HID_MOUSE_BTN_MIDDLE_RESET 0x0

// GAME - Game pad register
#define USB_HID_GAME_ADDR 0x18
#define USB_HID_GAME_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 32; // TBD
  };
} usb_hid_game_t;

// GAME.VALUE - TBD
#define USB_HID_GAME_VALUE_WIDTH 32
#define USB_HID_GAME_VALUE_LSB 0
#define USB_HID_GAME_VALUE_MASK 0xffffffff
#define USB_HID_GAME_VALUE_RESET 0x0

// REPORT_0 - USB report 0 register
#define USB_HID_REPORT_0_ADDR 0x1c
#define USB_HID_REPORT_0_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 32; // USB report 0 value
  };
} usb_hid_report_0_t;

// REPORT_0.VALUE - USB report 0 value
#define USB_HID_REPORT_0_VALUE_WIDTH 32
#define USB_HID_REPORT_0_VALUE_LSB 0
#define USB_HID_REPORT_0_VALUE_MASK 0xffffffff
#define USB_HID_REPORT_0_VALUE_RESET 0x0

// REPORT_1 - USB report 1 register
#define USB_HID_REPORT_1_ADDR 0x20
#define USB_HID_REPORT_1_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 32; // USB report 1 value
  };
} usb_hid_report_1_t;

// REPORT_1.VALUE - USB report 1 value
#define USB_HID_REPORT_1_VALUE_WIDTH 32
#define USB_HID_REPORT_1_VALUE_LSB 0
#define USB_HID_REPORT_1_VALUE_MASK 0xffffffff
#define USB_HID_REPORT_1_VALUE_RESET 0x0

// LEDS - LED register
#define USB_HID_LEDS_ADDR 0x24
#define USB_HID_LEDS_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t NUM_LOCK : 1; // Num lock LED
    uint32_t CAPS_LOCK : 1; // CAPS lock LED
    uint32_t SCROLL_LOCK : 1; // SCROLL lock LED
    uint32_t COMPOSE : 1; // Compose LED
    uint32_t : 28; // reserved
  };
} usb_hid_leds_t;

// LEDS.NUM_LOCK - Num lock LED
#define USB_HID_LEDS_NUM_LOCK_WIDTH 1
#define USB_HID_LEDS_NUM_LOCK_LSB 0
#define USB_HID_LEDS_NUM_LOCK_MASK 0x1
#define USB_HID_LEDS_NUM_LOCK_RESET 0x0

// LEDS.CAPS_LOCK - CAPS lock LED
#define USB_HID_LEDS_CAPS_LOCK_WIDTH 1
#define USB_HID_LEDS_CAPS_LOCK_LSB 1
#define USB_HID_LEDS_CAPS_LOCK_MASK 0x2
#define USB_HID_LEDS_CAPS_LOCK_RESET 0x0

// LEDS.SCROLL_LOCK - SCROLL lock LED
#define USB_HID_LEDS_SCROLL_LOCK_WIDTH 1
#define USB_HID_LEDS_SCROLL_LOCK_LSB 2
#define USB_HID_LEDS_SCROLL_LOCK_MASK 0x4
#define USB_HID_LEDS_SCROLL_LOCK_RESET 0x0

// LEDS.COMPOSE - Compose LED
#define USB_HID_LEDS_COMPOSE_WIDTH 1
#define USB_HID_LEDS_COMPOSE_LSB 3
#define USB_HID_LEDS_COMPOSE_MASK 0x8
#define USB_HID_LEDS_COMPOSE_RESET 0x0


// Register map structure
typedef struct {
    union {
        __IO uint32_t IEN; // Interrupt Enable Register
        __IO usb_hid_ien_t IEN_bf; // Bit access for IEN register
    };
    union {
        __IO uint32_t ISR; // Interrupt Status Register
        __IO usb_hid_isr_t ISR_bf; // Bit access for ISR register
    };
    union {
        __I uint32_t STATUS; // Status Register
        __I usb_hid_status_t STATUS_bf; // Bit access for STATUS register
    };
    union {
        __I uint32_t KEY_MODS; // Key modifiers
        __I usb_hid_key_mods_t KEY_MODS_bf; // Bit access for KEY_MODS register
    };
    union {
        __I uint32_t KEYS; // Keys register
        __I usb_hid_keys_t KEYS_bf; // Bit access for KEYS register
    };
    union {
        __I uint32_t MOUSE; // Mouse register
        __I usb_hid_mouse_t MOUSE_bf; // Bit access for MOUSE register
    };
    union {
        __I uint32_t GAME; // Game pad register
        __I usb_hid_game_t GAME_bf; // Bit access for GAME register
    };
    union {
        __I uint32_t REPORT_0; // USB report 0 register
        __I usb_hid_report_0_t REPORT_0_bf; // Bit access for REPORT_0 register
    };
    union {
        __I uint32_t REPORT_1; // USB report 1 register
        __I usb_hid_report_1_t REPORT_1_bf; // Bit access for REPORT_1 register
    };
    union {
        __IO uint32_t LEDS; // LED register
        __IO usb_hid_leds_t LEDS_bf; // Bit access for LEDS register
    };
} usb_hid_t;

#define USB_HID ((usb_hid_t*)(USB_HID_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __USB_HID_REGS_H */
