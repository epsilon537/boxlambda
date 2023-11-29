#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "platform.h"
#include "utils.h"
#include "usb_hid_hal.h"

#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);
}

//_exit is executed by the picolibc exit function. 
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

int main(void) {
  //Switches
  gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
  gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

  //Buttons
  gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
  gpio_set_direction(&gpio1, 0x00000000); //4 inputs

  unsigned usb0_status = 0;
  unsigned usb0_status_prev = 0;
  unsigned usb1_status = 0;
  unsigned usb1_status_prev = 0;
  unsigned usb0_typ=0, usb1_typ=0;
  unsigned key_mod=0;
  unsigned keys=0;
  unsigned mouse=0;
  unsigned game=0;
  unsigned rept[2] = {0,0};

  while (1) {
    usb0_status = usb_hid0_reg_rd(USB_HID_STATUS);
    usb0_typ = usb0_status & USB_HID_STATUS_USB_TYP_MSK;

    if (usb0_status != usb0_status_prev) {
      printf("USB0 status change: 0x%x\n", usb0_status);
      usb0_status_prev = usb0_status;
    }

    usb1_status = usb_hid1_reg_rd(USB_HID_STATUS);
    usb1_typ = usb1_status & USB_HID_STATUS_USB_TYP_MSK;
    if (usb1_status != usb1_status_prev) {
      printf("USB1 status change: 0x%x\n", usb1_status);
      usb1_status_prev = usb1_status;
    }

    unsigned isr0 = usb_hid0_reg_rd(USB_HID_ISR);
    usb_hid0_reg_wr(USB_HID_ISR, isr0);

    if (isr0) {
      rept[0] = usb_hid0_reg_rd(USB_HID_REPORT_0);
      rept[1] = usb_hid0_reg_rd(USB_HID_REPORT_1);

      printf("USB0 report: 0x%x%x\n", rept[1], rept[0]);
      switch (usb0_typ) {
        case USB_TYP_KEYB:
          key_mod = usb_hid0_reg_rd(USB_HID_KEY_MODS);
          keys = usb_hid0_reg_rd(USB_HID_KEYS);
          printf("USB0: Key mods: 0x%x Keys: 0x%x\n", key_mod, keys);
          break;
        case USB_TYP_MOUSE:
          mouse = usb_hid0_reg_rd(USB_HID_MOUSE);
          printf("USB0: Mouse: 0x%x\n", mouse);
          break;
        case USB_TYP_GAME:
          game = usb_hid0_reg_rd(USB_HID_GAME);
          printf("USB0: Game: 0x%x\n", game);
          break;
        default:
          break;
      }
    }

    unsigned isr1 = usb_hid1_reg_rd(USB_HID_ISR);
    usb_hid1_reg_wr(USB_HID_ISR, isr1);

    if (isr1) {
      rept[0] = usb_hid1_reg_rd(USB_HID_REPORT_0);
      rept[1] = usb_hid1_reg_rd(USB_HID_REPORT_1);

      printf("USB1 report: 0x%x%x\n", rept[1], rept[0]);
  
      switch (usb1_typ) {
        case USB_TYP_KEYB:
          key_mod = usb_hid1_reg_rd(USB_HID_KEY_MODS);
          keys = usb_hid1_reg_rd(USB_HID_KEYS);
          printf("USB1: Key mods: 0x%x Keys: 0x%x\n", key_mod, keys);
          break;
        case USB_TYP_MOUSE:
          mouse = usb_hid1_reg_rd(USB_HID_MOUSE);
          printf("USB1: Mouse: 0x%x\n", mouse);
          break;
        case USB_TYP_GAME:
          game = usb_hid1_reg_rd(USB_HID_GAME);
          printf("USB1: Game: 0x%x\n", game);
          break;
        default:
          break;
      }
    }
  }

  return 0;
}
