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
#include "interrupts.h"

#define LED_TO_SET USB_HID_LED_CAPS_LOCK

#define GPIO_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;
static struct gpio gpio;

static unsigned leds = 1;
static unsigned prevTimeClocks;

extern "C" {

  //_init is executed by picolibc startup code before main().
  void _init(void) {
    //Set up UART and tie stdio to it.
    uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
    uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
    set_stdio_to_uart(&uart0);

    mcycle_start();
    disable_all_irqs();
  }

  //_exit is executed by the picolibc exit function.
  //An implementation has to be provided to be able to user assert().
  void	_exit (int status) {
    while (1);
  }

  volatile unsigned usb0_usb_report_irq_fired = 0;
  volatile unsigned usb0_led_report_irq_fired = 0;
  volatile unsigned usb1_usb_report_irq_fired = 0;
  volatile unsigned usb1_led_report_irq_fired = 0;

  unsigned usb0_status_prev = 0;
  unsigned usb1_status_prev = 0;

  void _usb_hid_0_irq_handler(void) {
    unsigned ien = usb_hid_reg_rd(usb0, USB_HID_IEN);
    unsigned isr = usb_hid_reg_rd(usb0, USB_HID_ISR);
    if (isr & ien & USB_HID_IRQ_BIT_USB_REPORT) {
      //Acknowledge IRQ in the USB host core.
      usb_hid_reg_wr(usb0, USB_HID_ISR, USB_HID_IRQ_BIT_USB_REPORT);
      usb0_usb_report_irq_fired = 1;
    }

    if (isr & ien & USB_HID_IRQ_BIT_LED_REPORT) {
      //Acknowledge IRQ in the USB host core.
      usb_hid_reg_wr(usb0, USB_HID_ISR, USB_HID_IRQ_BIT_LED_REPORT);
      usb0_led_report_irq_fired = 1;
    }
  }

  void _usb_hid_1_irq_handler(void) {
    unsigned ien = usb_hid_reg_rd(usb1, USB_HID_IEN);
    unsigned isr = usb_hid_reg_rd(usb1, USB_HID_ISR);
    if (isr & ien & USB_HID_IRQ_BIT_USB_REPORT) {
      usb_hid_reg_wr(usb1, USB_HID_ISR, USB_HID_IRQ_BIT_USB_REPORT);
      usb1_usb_report_irq_fired = 1;
    }

    if (isr & ien & USB_HID_IRQ_BIT_LED_REPORT) {
      usb_hid_reg_wr(usb1, USB_HID_ISR, USB_HID_IRQ_BIT_LED_REPORT);
      usb1_led_report_irq_fired = 1;
    }
  }

}

//Returns usb_status
static void check_usb(USB_HID_Host_t *usb, unsigned &usb_status_prev, volatile unsigned &usb_report_irq_fired, volatile unsigned &led_report_irq_fired) {

  assert(usb);

  unsigned usb_status = usb_hid_reg_rd(usb, USB_HID_STATUS);
  unsigned usb_typ = usb_status & USB_HID_STATUS_USB_TYP_MSK;

  if (usb_status != usb_status_prev) {
    printf("USB%d: Status change: 0x%x -> 0x%x\n", usb->id, usb_status_prev, usb_status);

    if (usb_status & USB_HID_STATUS_CONN_ERR_BIT) {
      printf("  Connection Error!\n");
    }

    switch (usb_status & USB_HID_STATUS_USB_TYP_MSK) {
      case USB_TYP_KEYB:
        printf("  Keyboard detected.\n");
        break;
      case USB_TYP_MOUSE:
        printf("  Mouse detected.\n");
        break;
      case USB_TYP_GAME:
        printf("  Gamepad detected.\n");
        break;
      default:
        printf("  Unknown device detected.\n");
        break;
    }

    usb_status_prev = usb_status;
  }

  if ((gpio_get_input(&gpio) & 0x10) && (usb_typ == USB_TYP_KEYB)) {
    unsigned curTimeClocks = mcycle_get32();

    //Every 100ms...
    if (cc2us(curTimeClocks - prevTimeClocks) >= 100000) {
      leds <<= 1;
      if (leds == 8) {
        leds = 1;
      }

      usb_hid_reg_wr(usb, USB_HID_LEDS, leds);

      //The USB host core confirms the LED update with an IRQ.
      int counter = 0;
      while (!led_report_irq_fired) {
        usleep(1000);

        ++counter;
        if (counter > 1000) {
          printf("Led Report IRQ not received.\n");
        }
      }

      led_report_irq_fired = 0;

      prevTimeClocks = curTimeClocks;
    }
  }

  //Did we receive a new report (signalled via IRQ)?
  if (usb_report_irq_fired) {
    unsigned key_mod;
    unsigned keys;
    unsigned mouse;
    unsigned game;
    USB_HID_Report_t report;

    usb_report_irq_fired = 0;

    usb_hid_get_report(usb, &report);

    switch (usb_typ) {
      case USB_TYP_KEYB:
        printf("USB%d keyboard report: 0x%x%x\n", usb->id, report.report0, report.report1);
        key_mod = usb_hid_reg_rd(usb, USB_HID_KEY_MODS);
        keys = usb_hid_reg_rd(usb, USB_HID_KEYS);
        printf("  Key mods: 0x%x Keys: 0x%x\n", key_mod, keys);
        break;
      case USB_TYP_MOUSE:
        printf("USB%d mouse report: 0x%x%x\n", usb->id, report.report0, report.report1);
        mouse = usb_hid_reg_rd(usb, USB_HID_MOUSE);
        printf("  Mouse: 0x%x\n", mouse);
        break;
      case USB_TYP_GAME:
        printf("USB%d gamepad report: 0x%x%x\n", usb->id, report.report0, report.report1);
        game = usb_hid_reg_rd(usb, USB_HID_GAME);
        printf("  Game: 0x%x\n", game);
        break;
      default:
        printf("USB%d unknown report: 0x%x%x\n", usb->id, report.report0, report.report1);
        break;
    }
  }
}

int main(void) {
  //Switches
  gpio_init(&gpio, (volatile void *)GPIO_BASE);
  gpio_set_direction(&gpio, 0x0000000F); //4 inputs, 4 outputs

  printf("Enabling IRQs\n");
  enable_global_irq(); //Enable global IRQ line at the CPU
  enable_irq(IRQ_ID_USB_HID_0); //Enable USB IRQ at the CPU
  enable_irq(IRQ_ID_USB_HID_1);
  usb_hid_reg_wr(usb0, USB_HID_ISR, 3); //Clear any pending IRQs in the USB host core.
  usb_hid_reg_wr(usb1, USB_HID_ISR, 3);
  usb_hid_reg_wr(usb0, USB_HID_IEN, 3); //Enable LED and report IRQ in the USB host core.
  usb_hid_reg_wr(usb1, USB_HID_IEN, 3);

  //Initialize 'previous time' for LED update tracking.
  prevTimeClocks = mcycle_get32();

  printf("USB HID Test Start.\n");

  while (1) {
    check_usb(usb0, usb0_status_prev, usb0_usb_report_irq_fired, usb0_led_report_irq_fired);
    check_usb(usb1, usb1_status_prev, usb1_usb_report_irq_fired, usb1_led_report_irq_fired);
  }

  return 0;
}

