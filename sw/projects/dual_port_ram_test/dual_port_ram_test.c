/*Dual Port RAM test checks code and data access to the CMEM and DMEM dual port memories.
 *It also check simultaneous access to those memories from the CPU and PICORV DMA.*/
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

/*PicoRV register mapping for the wordcopy_burst program.*/
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

/*Create some data buffers in CMEM by mapping them to the .cmem_bss segment.*/
char cmem_str[32] __attribute__ ((section (".cmem_bss")));
int cmem_buf_0[256]  __attribute__ ((section (".cmem_bss")));
int cmem_buf_1[256]  __attribute__ ((section (".cmem_bss")));
/*Also create some data buffers in DMEM (default BSS segment).*/
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

    return crc;
}

void waitDMAcomplete() {
    int dmaBusy = DMA_BUSY;
    while(dmaBusy) {
        dmaBusy = picorv_hir_reg_rd(PICORV_HIR_REG_CTRL_STAT);
    }
}

void dmaCopyStart(unsigned char* srcPtr, unsigned char* dstPtr, unsigned numElems, unsigned elemSize) {
    /*Or-in 0x100000 to make sure PicoRV DMA access to the Dual Port memories is done through the 'other' port, i.e.
     *not the port the CPU is using.*/
    picorv_hir_reg_wr(PICORV_HIR_REG_SRC, (unsigned)srcPtr | 0x100000);
    picorv_hir_reg_wr(PICORV_HIR_REG_DST, (unsigned)dstPtr | 0x100000);
    picorv_hir_reg_wr(PICORV_HIR_REG_NUM_ELEMS, numElems);

    picorv_hir_reg_wr(PICORV_HIR_REG_CTRL_STAT, DMA_START);    
}

int dualCopyTest(unsigned* src_0, unsigned* dst_0, unsigned* src_1, unsigned* dst_1) {
    //Fill source buffers with some random data and destination with 0x55s
    for (int ii=0; ii<256; ii++) {
        src_0[ii] = (unsigned)rand();
        src_1[ii] = (unsigned)rand();
        dst_0[ii] = 0x55555555;
        dst_1[ii] = 0x55555555;
    }

    //Kick off DMA.
    dmaCopyStart((unsigned char*)src_0, (unsigned char*)dst_0, 256, sizeof(int)); 
    
    //Simultaneously do a memcpy.
    for (int ii=0; ii<256; ii++) {
        dst_1[ii] = src_1[ii];
    }
    
    waitDMAcomplete();

    //Check the copy.
    if (memcmp(src_0, dst_0, 256*sizeof(unsigned))) {
        printf("DMA copy failed.\n");

        for (int ii=0; ii<256; ii++) {
            printf("[%d] 0x%x vs. 0x%x\n", ii, src_0[ii], dst_0[ii]);
        }

        return -1;
    }

    if (memcmp(src_1, dst_1, 256*sizeof(unsigned))) {
        printf("Memcpy failed.\n");

        for (int ii=0; ii<256; ii++) {
            printf("[%d] 0x%x vs. 0x%x\n", ii, src_0[ii], dst_0[ii]);
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
