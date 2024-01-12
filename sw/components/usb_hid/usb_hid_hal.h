#ifndef USB_HID_HAL_H
#define USB_HID_HAL_H

#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * USB HID hardware access layer.
 */
#define USB_HID0_BASE 0x10000040
#define USB_HID1_BASE 0x10000080

//USB HID register offsets and their fields
#define USB_HID_IEN 0
#define USB_HID_ISR 1
#define USB_HID_IRQ_BIT_USB_REPORT (1<<0)

#define USB_HID_STATUS 2
#define USB_HID_STATUS_CONN_ERR_BIT (1<<2)
#define USB_HID_STATUS_USB_TYP_MSK (3<<0)
#define USB_TYP_NO_DEV 0
#define USB_TYP_KEYB 1
#define USB_TYP_MOUSE 2
#define USB_TYP_GAME 3

#define USB_HID_KEY_MODS 3
#define USB_HID_KEY_MOD_MSK (255<<0)

#define USB_HID_KEYS 4
#define USB_HID_KEY_0_MSK (255<<0)
#define USB_HID_KEY_1_MSK (255<<8)
#define USB_HID_KEY_2_MSK (255<<16)
#define USB_HID_KEY_3_MSK (255<<24)

#define USB_HID_MOUSE 5
#define USB_HID_MOUSE_DY_MSK (255<<0)
#define USB_HID_MOUSE_DX_MSK (255<<8)
#define USB_HID_MOUSE_BTN_LEFT_BIT (1<<16)
#define USB_HID_MOUSE_BTN_RIGHT_BIT (1<<17)
#define USB_HID_MOUSE_BTN_MIDDLE_BIT (1<<18)

#define USB_HID_GAME 6

#define USB_HID_REPORT_0 7
#define USB_HID_REPORT_1 8

//Write to Register
inline void usb_hid0_reg_wr(unsigned reg_offset, unsigned val)
{
	assert(reg_offset <= USB_HID_REPORT_1);
	((unsigned volatile *)(USB_HID0_BASE))[reg_offset] = val;
}

//Read from Register
inline unsigned usb_hid0_reg_rd(unsigned reg_offset)
{
	assert(reg_offset <= USB_HID_REPORT_1);
	return ((unsigned volatile *)(USB_HID0_BASE))[reg_offset];
}

//Write to Register
inline void usb_hid1_reg_wr(unsigned reg_offset, unsigned val)
{
	assert(reg_offset <= USB_HID_REPORT_1);
	((unsigned volatile *)(USB_HID1_BASE))[reg_offset] = val;
}

//Read from Register
inline unsigned usb_hid1_reg_rd(unsigned reg_offset)
{
	assert(reg_offset <= USB_HID_REPORT_1);
	return ((unsigned volatile *)(USB_HID1_BASE))[reg_offset];
}

#ifdef __cplusplus
}
#endif

#endif //USB_HID_HAL_H