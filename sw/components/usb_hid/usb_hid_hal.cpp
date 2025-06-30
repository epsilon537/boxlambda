#include "usb_hid_hal.h"

void usb_hid_get_report(usb_hid_t *usb, USB_HID_Report_t *report) {
    assert(report);

    report->report0 = usb->REPORT_0;
    report->report1 = usb->REPORT_1;
}
