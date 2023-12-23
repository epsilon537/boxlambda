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

#define PICORV_HIR_REG_SRC PICORV_HIR_0
#define PICORV_HIR_REG_DST PICORV_HIR_1
#define PICORV_HIR_REG_NUM_ELEMS PICORV_HIR_2
#define PICORV_HIR_REG_CTRL_STAT PICORV_HIR_3

#define DMA_START 1
#define DMA_BUSY 2

//PicoRV copy programs
#include "picorv_wordcopy_burst.h"
#include "picorv_dma_hal.h"

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

char cmem_str[32] __attribute__ ((section (".cmem_bss")));
int cmem_buf_0[256]  __attribute__ ((section (".cmem_bss")));
int cmem_buf_1[256]  __attribute__ ((section (".cmem_bss")));
int dmem_buf_0[256];
int dmem_buf_1[256];

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

static unsigned int crc;

/*This function executes from DMEM. It doesn't matter it does. What matters is that it executes from DMEM.*/
__attribute__ ((section(".dmem_text")))
int code_in_dmem(char *message) {
  int i, j;
  unsigned int byte, mask;

   i = 0;
   crc = 0xFFFFFFFF;
   while (i<16) {
      byte = message[i];            // Get next byte.
      crc = crc ^ byte;
      for (j = 7; j >= 0; j--) {    // Do eight times.
         mask = -(crc & 1);
         crc = (crc >> 1) ^ (0xEDB88320 & mask);
      }
      i = i + 1;
   }

   return crc;
}

void waitDMAcomplete() {
    int dmaBusy = DMA_BUSY;
    while(dmaBusy) {
        dmaBusy = picorv_hir_reg_rd(PICORV_HIR_REG_CTRL_STAT);
    }
}

void dmaCopyStart(unsigned char* srcBase, unsigned srcOffset, unsigned char* dstBase, unsigned dstOffset, 
        unsigned numElems, unsigned elemSize) {
    unsigned char* srcPtr = srcBase + srcOffset*elemSize;
    //Offset destination pointer deeper into its buffer so we have room to check for out-of-bounds accesses before
    //the destination start pointer.
    unsigned char* dstPtr = dstBase + dstOffset*elemSize;

    picorv_hir_reg_wr(PICORV_HIR_REG_SRC, (unsigned)srcPtr | 0x100000);
    picorv_hir_reg_wr(PICORV_HIR_REG_DST, (unsigned)dstPtr | 0x100000);
    picorv_hir_reg_wr(PICORV_HIR_REG_NUM_ELEMS, numElems);

    picorv_hir_reg_wr(PICORV_HIR_REG_CTRL_STAT, DMA_START);    
}

int dualCopyTest(unsigned* src_0, unsigned* dst_0, unsigned* src_1, unsigned* dst_1) {
    //Fill source buffers with some random data and destination with 0x55s
    for (int ii=0; ii<256*4; ii++) {
        ((char*)src_0)[ii] = (unsigned char)rand();
        ((char*)src_1)[ii] = (unsigned char)rand();
        ((char*)dst_0)[ii] = 0x55;
        ((char*)dst_1)[ii] = 0x55;
    }

    //Kick off DMA
    dmaCopyStart((char*)src_0, 0, (char*)dst_0, 0, 256, sizeof(int)); 
    
    //Simultaneously do a memcpy
    for (int ii=0; ii<256; ii++) {
        dst_1[ii] = src_1[ii];
    }
    
    waitDMAcomplete();

    //Check the copy.
    if (memcmp(src_0, dst_0, 256*sizeof(int))) {
        printf("DMA copy failed.\n");

        for (int ii=0; ii<256*sizeof(int); ii++) {
            printf("[%d] 0x%x vs. 0x%x\n", ii, (char*)(src_0)[ii], (char*)(dst_0)[ii]);
        }

        return -1;
    }

    if (memcmp(src_1, dst_1, 256*sizeof(int))) {
        printf("Memcpy failed.\n");

        for (int ii=0; ii<256*sizeof(int); ii++) {
            printf("[%d] 0x%x vs. 0x%x\n", ii, (char*)(src_0)[ii], (char*)(dst_0)[ii]);
        }

        return -1;
    }

    return 0;
}

int main(void) {
    //Switches
    gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
    gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

    //Buttons
    gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
    gpio_set_direction(&gpio1, 0x00000000); //4 inputs

    printf("Executing code from DMEM...\n");
    code_in_dmem((char*)&code_in_dmem);

    printf("CMEM data access...\n");

    char *test_str = "Hello CMEM!";

    strcpy(cmem_str, test_str);
    printf("%s\n", cmem_str);
    if (strcmp(test_str, cmem_str)) {
        printf("CMEM data access failed!\n");
        return -1;
    }
    
    printf("Load PicoRV Program wordcopy_burst\n");

    picorv_load_program(picorv_wordcopy_burst_picobin, sizeof(picorv_wordcopy_burst_picobin));

    printf("Taking PicoRV out of reset...\n");
    picorv_sys_reg_wr(PICORV_SYS_REG_CTRL, 1);

    printf("Dual copy from DMEM to CMEM...\n");

    if (dualCopyTest(dmem_buf_0, cmem_buf_0, dmem_buf_1, cmem_buf_1)) {
        return -1;
    }

    printf("Dual copy from CMEM to DMEM...\n");

    if (dualCopyTest(cmem_buf_0, dmem_buf_0, cmem_buf_1, dmem_buf_1)) {
        return -1;
    }

    printf("Test Successful.\n");

    return 0;
}
