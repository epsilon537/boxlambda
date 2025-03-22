/*Dual Port RAM test checks code and data access to the CMEM and DMEM dual port memories.*/
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"

static struct uart uart0;
static struct gpio gpio;

/*Create some data buffers in CMEM by mapping them to the .cmem_bss segment.*/
char cmem_str[32] __attribute__ ((section (".cmem_bss")));
volatile int cmem_buf_0[256]  __attribute__ ((section (".cmem_bss")));
volatile int cmem_buf_1[256]  __attribute__ ((section (".cmem_bss")));
/*Also create some data buffers in DMEM (default BSS segment).*/
volatile int dmem_buf_0[256];
volatile int dmem_buf_1[256];

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

/*This function executes from DMEM. It doesn't matter what it does. What matters is that it executes from DMEM.*/
__attribute__ ((section(".dmem_text")))
int code_in_dmem(char *message) {
    int i, j;
    unsigned int byte, mask;
    unsigned int crc;

    i = 0;
    crc = 0xFFFFFFFF;
    while (i<16) {
        byte = message[i];
        crc = crc ^ byte;
        for (j = 7; j >= 0; j--) {
            mask = -(crc & 1);
            crc = (crc >> 1) ^ (0xEDB88320 & mask);
        }
        i = i + 1;
    }

    printf("Returning back to CMEM.\n");

    return crc;
}

int main(void) {
    //Switches
    gpio_init(&gpio, (volatile void *)GPIO_BASE);
    gpio_set_direction(&gpio, 0x0000000F); //4 inputs, 4 outputs

    gpio_clear_pin(&gpio, 1);

    printf("Executing code from DMEM... (0x%x)\n", &code_in_dmem);
    code_in_dmem((char*)&code_in_dmem);

    printf("CMEM word copy...\n");

    volatile int *src = cmem_buf_0;
    volatile int *dst = cmem_buf_1;
    volatile int *end = &src[256];

    gpio_set_pin(&gpio, 1);

    while (src < end) {
        *dst = *src;
        ++src;
        ++dst;
    }

    gpio_clear_pin(&gpio, 1);

    printf("DMEM word copy...\n");

    src = dmem_buf_0;
    dst = dmem_buf_1;
    end = &src[256];

    gpio_set_pin(&gpio, 1);

    while (src < end) {
        *dst = *src;
        ++src;
        ++dst;
    }

    gpio_clear_pin(&gpio, 1);

    char *test_str = "Hello CMEM!";
    strcpy(cmem_str, test_str);
    printf("%s\n", cmem_str);
    if (strcmp(test_str, cmem_str)) {
        printf("CMEM data access failed!\n");
        return -1;
    }

    printf("Test Successful.\n");

    return 0;
}
