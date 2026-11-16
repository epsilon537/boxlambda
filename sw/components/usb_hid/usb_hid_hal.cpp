#include "usb_hid_hal.h"
#include "reset_regs.h"

void usb_hid_0_get_report(USB_HID_Report_t *report) {
  assert(report);

  report->report0 = USB_HID_0->REPORT_0;
  report->report1 = USB_HID_0->REPORT_1;
}

void usb_hid_1_get_report(USB_HID_Report_t *report) {
  assert(report);

  report->report0 = USB_HID_0->REPORT_0;
  report->report1 = USB_HID_0->REPORT_1;
}

void usb_hid_reset() { RESET->CONTROL = RESET_CONTROL_CTRL_USB_RESET; }
