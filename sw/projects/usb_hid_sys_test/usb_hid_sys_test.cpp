#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "usb_hid_hal.h"
#include "interrupts.h"

#define LED_TO_SET USB_HID_LED_CAPS_LOCK

#define GPIO_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static uint32_t leds = 1;
static uint32_t prevTimeClocks;

extern "C" {

  //_init is executed by picolibc startup code before main().
  void _init(void) {
    mcycle_start();
    disable_all_irqs();
  }

  //_exit is executed by the picolibc exit function.
  //An implementation has to be provided to be able to user assert().
  void	_exit (int status) {
    while (1);
  }

  volatile uint32_t usb0_usb_report_irq_fired = 0;
  volatile uint32_t usb0_led_report_irq_fired = 0;
  volatile uint32_t usb1_usb_report_irq_fired = 0;
  volatile uint32_t usb1_led_report_irq_fired = 0;

  uint32_t usb0_status_prev = 0;
  uint32_t usb1_status_prev = 0;

  void _usb_hid_0_irq_handler(void) {
    uint32_t ien = USB_HID_0->IEN;
    uint32_t isr = USB_HID_0->ISR;
    if (isr & ien & USB_HID_ISR_USB_REPORT_MASK) {
      //Acknowledge IRQ in the USB host core.
      USB_HID_0->ISR_bf.USB_REPORT = 1;
      usb0_usb_report_irq_fired = 1;
    }

    if (isr & ien & USB_HID_ISR_LED_REPORT_MASK) {
      //Acknowledge IRQ in the USB host core.
      USB_HID_0->ISR_bf.LED_REPORT = 1;
      usb0_led_report_irq_fired = 1;
    }

    //Return from interrupt
    __asm__ volatile (
        "mret \n"
    );

    while (1);
  }

  void _usb_hid_1_irq_handler(void) {
    uint32_t ien = USB_HID_1->IEN;
    uint32_t isr = USB_HID_1->ISR;
    if (isr & ien & USB_HID_ISR_USB_REPORT_MASK) {
      //Acknowledge IRQ in the USB host core.
      USB_HID_1->ISR_bf.USB_REPORT = 1;
      usb1_usb_report_irq_fired = 1;
    }

    if (isr & ien & USB_HID_ISR_LED_REPORT_MASK) {
      //Acknowledge IRQ in the USB host core.
      USB_HID_1->ISR_bf.LED_REPORT = 1;
      usb1_led_report_irq_fired = 1;
    }

    //Return from interrupt
    __asm__ volatile (
        "mret \n"
    );

    while (1);
  }
}

//Returns usb_status
static void check_usb(usb_hid_t *usb, uint32_t &usb_status_prev, volatile uint32_t &usb_report_irq_fired, volatile uint32_t &led_report_irq_fired) {

  assert(usb);

  uint32_t usb_status = usb->STATUS;
  uint32_t usb_typ = usb_status & USB_HID_STATUS_USB_TYP_MASK;

  if (usb_status != usb_status_prev) {
    printf("USB%d: Status change: 0x%x -> 0x%x\n", usb_id(usb), usb_status_prev, usb_status);

    if (usb_status & USB_HID_STATUS_CONN_ERR_MASK) {
      printf("  Connection Error!\n");
    }

    switch (usb_typ) {
      case USB_HID_STATUS_USB_TYP_KEYB:
        printf("  Keyboard detected.\n");
        break;
      case USB_HID_STATUS_USB_TYP_MOUSE:
        printf("  Mouse detected.\n");
        break;
      case USB_HID_STATUS_USB_TYP_GAME:
        printf("  Gamepad detected.\n");
        break;
      default:
        printf("  Unknown device detected.\n");
        break;
    }

    usb_status_prev = usb_status;
  }

  if ((gpio_get_input() & 0x10) && (usb_typ == USB_HID_STATUS_USB_TYP_KEYB)) {
    uint32_t curTimeClocks = mcycle_get32();

    //Every 100ms...
    if (cc2us(curTimeClocks - prevTimeClocks) >= 100000) {
      leds <<= 1;
      if (leds == 8) {
        leds = 1;
      }

      usb->LEDS = leds;

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
    uint32_t key_mod;
    uint32_t keys;
    uint32_t mouse;
    uint32_t game;
    USB_HID_Report_t report;

    usb_report_irq_fired = 0;

    usb_hid_get_report(usb, &report);

    //Re-read status. Old usb_status value might be stale.
    usb_status = usb->STATUS;
    usb_typ = usb_status & USB_HID_STATUS_USB_TYP_MASK;

    printf("USB%d report received: 0x08%x08%x\n", usb_id(usb), report.report0, report.report1);
    switch (usb_typ) {
      case USB_HID_STATUS_USB_TYP_KEYB:
        key_mod = usb->KEY_MODS;
        keys = usb->KEYS;
        printf("  Keyboard report: Key mods: 0x%x Keys: 0x%x\n", key_mod, keys);
        break;
      case USB_HID_STATUS_USB_TYP_MOUSE:
        mouse = usb->MOUSE;
        printf("  Mouse report: 0x%x\n", mouse);
        break;
      case USB_HID_STATUS_USB_TYP_GAME:
        game = usb->GAME;
        printf("  Game report: 0x%x\n", game);
        break;
      default:
        printf("  Unknown device\n");
        break;
    }
  }
}

int main(void) {
  //Switches
  gpio_init();
  gpio_set_direction(0x0000000F); //4 inputs, 4 outputs

  printf("Enabling IRQs\n");
  enable_global_irq(); //Enable global IRQ line at the CPU
  enable_irq(IRQ_ID_USB_HID_0); //Enable USB IRQ at the CPU
  enable_irq(IRQ_ID_USB_HID_1);

  //Clear any pending IRQs in the USB host core.
  USB_HID_0->ISR_bf.USB_REPORT = 1;
  USB_HID_0->ISR_bf.LED_REPORT = 1;
  USB_HID_1->ISR_bf.USB_REPORT = 1;
  USB_HID_1->ISR_bf.LED_REPORT = 1;
  //Enable LED and report IRQ in the USB host core.
  USB_HID_0->IEN_bf.USB_REPORT = 1;
  USB_HID_0->IEN_bf.LED_REPORT = 1;
  USB_HID_1->IEN_bf.USB_REPORT = 1;
  USB_HID_1->IEN_bf.LED_REPORT = 1;

  //Initialize 'previous time' for LED update tracking.
  prevTimeClocks = mcycle_get32();

  printf("USB HID Test Start.\n");

  while (1) {
    check_usb(USB_HID_0, usb0_status_prev, usb0_usb_report_irq_fired, usb0_led_report_irq_fired);
    check_usb(USB_HID_1, usb1_status_prev, usb1_usb_report_irq_fired, usb1_led_report_irq_fired);
  }

  return 0;
}

