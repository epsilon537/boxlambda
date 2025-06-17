#ifndef USB_HID_HAL_H
#define USB_HID_HAL_H

#include <assert.h>
#include "usb_hid_regs.h"

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
#define USB_HID_0_BASE_ADDR 0x10000040
#define USB_HID_1_BASE_ADDR 0x10000080

#define USB_HID_0 ((usb_hid_t*)(USB_HID_0_BASE_ADDR))
#define USB_HID_1 ((usb_hid_t*)(USB_HID_1_BASE_ADDR))

//USB report data structure.
typedef struct USB_HID_Report_t {
	unsigned report0;
	unsigned report1;
} USB_HID_Report_t;

//Retrieve the latest USB report.
void usb_hid_get_report(IN usb_hid_t *usb, OUT USB_HID_Report_t *report);

//Returns usb instance number (0 or 1)
inline uint32_t usb_id(IN usb_hid_t *usb) {
  if (usb == USB_HID_0) return 0;
  if (usb == USB_HID_1) return 1;
  assert(0);
  return -1;
}

#ifdef __cplusplus
}
#endif

#endif //USB_HID_HAL_H
