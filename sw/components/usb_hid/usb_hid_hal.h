#ifndef USB_HID_HAL_H
#define USB_HID_HAL_H

#include "inout.h"
#include "usb_hid_0_regs.h"
#include "usb_hid_1_regs.h"
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * USB HID hardware access layer.
 */

// USB report data structure.
typedef struct USB_HID_Report_t {
  uint32_t report0;
  uint32_t report1;
} USB_HID_Report_t;

// Retrieve the latest USB report.
void usb_hid_0_get_report(OUT USB_HID_Report_t *report);
void usb_hid_1_get_report(OUT USB_HID_Report_t *report);

#ifdef __cplusplus
}
#endif

#endif // USB_HID_HAL_H
