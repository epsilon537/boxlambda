#ifndef USB_HID_HAL_H
#define USB_HID_HAL_H

#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef IN
#define IN
#endif

#ifndef OUT
#define OUT
#endif

#ifndef INOUT
#define INOUT
#endif

/*
 * USB HID hardware access layer.
 */
#define USB_HID0_BASE 0x10000040
#define USB_HID1_BASE 0x10000080

//USB HID register offsets and their fields
//Register:
#define USB_HID_IEN 0
//Register:
#define USB_HID_ISR 1
//Fields:
#define USB_HID_IRQ_BIT_USB_REPORT (1<<0)
#define USB_HID_IRQ_BIT_LED_REPORT (1<<1)
//Register:
#define USB_HID_STATUS 2
//Fields:
#define USB_HID_STATUS_CONN_ERR_BIT (1<<2)
#define USB_HID_STATUS_USB_TYP_MSK (3<<0)
#define USB_TYP_NO_DEV 0
#define USB_TYP_KEYB 1
#define USB_TYP_MOUSE 2
#define USB_TYP_GAME 3

//Register:
#define USB_HID_KEY_MODS 3
//Fields:
#define USB_HID_KEY_MOD_MSK (255<<0)
#define USB_HID_KEY_MOD_LCTRL  0x01
#define USB_HID_KEY_MOD_LSHIFT 0x02
#define USB_HID_KEY_MOD_LALT   0x04
#define USB_HID_KEY_MOD_LMETA  0x08
#define USB_HID_KEY_MOD_RCTRL  0x10
#define USB_HID_KEY_MOD_RSHIFT 0x20
#define USB_HID_KEY_MOD_RALT   0x40
#define USB_HID_KEY_MOD_RMETA  0x80

//Register:
#define USB_HID_KEYS 4
//Fields:
#define USB_HID_KEY_0_MSK (255<<0)
#define USB_HID_KEY_1_MSK (255<<8)
#define USB_HID_KEY_2_MSK (255<<16)
#define USB_HID_KEY_3_MSK (255<<24)

//Register:
#define USB_HID_MOUSE 5
//Fields:
#define USB_HID_MOUSE_DY_MSK (255<<0)
#define USB_HID_MOUSE_DX_MSK (255<<8)
#define USB_HID_MOUSE_BTN_LEFT_BIT (1<<16)
#define USB_HID_MOUSE_BTN_RIGHT_BIT (1<<17)
#define USB_HID_MOUSE_BTN_MIDDLE_BIT (1<<18)

//Register:
#define USB_HID_GAME 6

//Register:
#define USB_HID_REPORT_0 7
#define USB_HID_REPORT_1 8

//Keyboard LEDs register. Writes to this register will be forwarded to the USB device using a SetProtocol transaction.
//Register:
#define USB_HID_LEDS 9
//Fields:
#define USB_HID_LED_NUM_LOCK (1<<0)
#define USB_HID_LED_CAPS_LOCK (1<<1)
#define USB_HID_LED_SCROLL_LOCK (1<<2)
#define USB_HID_LED_COMPOSE (1<<3)

#define USB_HID_MAX_REG_OFFSET USB_HID_LEDS

//USB HID host data structure associated with a USB port.
typedef struct USB_HID_Host_t {
	unsigned volatile *base_addr;
	unsigned id;
} USB_HID_Host_t;

/*These two are initialized in usb_hid_hal.c*/
extern USB_HID_Host_t *usb0; //USB port 0.
extern USB_HID_Host_t *usb1; //USB port 1.

//Write to Register
static inline void usb_hid_reg_wr(IN USB_HID_Host_t *usb, unsigned reg_offset, unsigned val)
{
	assert(usb);
	assert(reg_offset <= USB_HID_MAX_REG_OFFSET);
	usb->base_addr[reg_offset] = val;
}

//Read from Register
static inline unsigned usb_hid_reg_rd(IN USB_HID_Host_t *usb, unsigned reg_offset)
{
	assert(usb);
	assert(reg_offset <= USB_HID_MAX_REG_OFFSET);
	return usb->base_addr[reg_offset];
}

//USB report data structure.
typedef struct USB_HID_Report_t {
	unsigned report0;
	unsigned report1;
} USB_HID_Report_t;

//Retrieve the latest USB report.
void usb_hid_get_report(IN USB_HID_Host_t *usb, OUT USB_HID_Report_t *report);

#ifdef __cplusplus
}
#endif

#endif //USB_HID_HAL_H