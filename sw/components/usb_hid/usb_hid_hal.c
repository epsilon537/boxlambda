#include "usb_hid_hal.h"

//Initialize USB port 0 and 1 data structures.
USB_HID_Host_t usb_host_0 = {(unsigned volatile *)(USB_HID0_BASE), 0};
USB_HID_Host_t usb_host_1 = {(unsigned volatile *)(USB_HID1_BASE), 1};

USB_HID_Host_t *usb0 = &usb_host_0;
USB_HID_Host_t *usb1 = &usb_host_1;

void usb_hid_get_report(USB_HID_Host_t *usb, USB_HID_Report_t *report) {
    assert(report);

    report->report0 = usb_hid_reg_rd(usb, USB_HID_REPORT_0);
    report->report1 = usb_hid_reg_rd(usb, USB_HID_REPORT_1);
}