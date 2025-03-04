#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"

//PicoRV copy programs
#include "picorv_wordcopy_single.h"
#include "picorv_wordcopy_burst.h"
#include "picorv_bytecopy_single.h"
#include "picorv_bytecopy_burst.h"
#include "picorv_dma_hal.h"
#include "sdram.h"
#include "vera_hal.h"
#include "interrupts.h"

/*This test program is a multiple-nested-loop iterating over various vectors (PicoRV program, src aligned, dst alignment
 *number of elements to copy, etc.). At the heart of the loop is a parameterized DMA copy routine and some checks verifying
 *that the copy was complete without going out of bounds.
 *Through various defines, the test vectors can be constrained to make test exeuction time more manageable.
 */

// Limit testing to aligned pointers only.
// #define ALIGNED_ONLY

// Stop test execution as soon as a failure is encountered.
#define STOP_ON_FAIL

// Enable detailed logging.
//#define DETAILED_LOGS

//Define or undefine these to limit to byte or wordcopy only.
//#define BYTE_COPY
#define WORD_COPY

//Define or undefine these to limit to single or burst mode only.
#define SINGLE_COPY
#define BURST_COPY

//Define or undefine these to limit src and dest memory types.
#define LOCAL_MEM_COPY
#define EXT_MEM0_COPY
#define VRAM_MEM_COPY

//Define or undefine these to limit src and dest offset test vectors.
#define OFFSET_0
#define OFFSET_1
//#define OFFSET_2
#define OFFSET_3

#ifdef ALIGNED_ONLY
#undef OFFSET_1
#undef OFFSET_2
#undef OFFSET_3
#endif

#define GPIO_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

#define PICORV_HIR_REG_SRC PICORV_HIR_0
#define PICORV_HIR_REG_DST PICORV_HIR_1
#define PICORV_HIR_REG_NUM_ELEMS PICORV_HIR_2
#define PICORV_HIR_REG_CTRL_STAT PICORV_HIR_3

#define DMA_START 1
#define DMA_BUSY 2

static struct uart uart0;
static struct gpio gpio;

//Some data bufers to copy from/to.
static unsigned srcBufLocal[64], dstBufLocal[64];

//A struct hold PicoRV program attributes.
typedef struct {
    const char *name;
    unsigned char *progDat;
    unsigned progLen;
    unsigned elemSize;
} Program;

//PicoRV program test vector.
Program programs[] = {
#ifdef WORD_COPY
#ifdef SINGLE_COPY
    {"WORDCOPY_SINGLE", picorv_wordcopy_single_picobin, sizeof(picorv_wordcopy_single_picobin), 4},
#endif
#ifdef BURST_COPY
    {"WORDCOPY_BURST", picorv_wordcopy_burst_picobin, sizeof(picorv_wordcopy_burst_picobin), 4},
#endif
#endif /*WORD_COPY*/
#ifdef BYTE_COPY
#ifdef SINGLE_COPY
    {"BYTECOPY_SINGLE", picorv_bytecopy_single_picobin, sizeof(picorv_bytecopy_single_picobin), 1},
#endif
#ifdef BURST_COPY
    {"BYTECOPY_BURST", picorv_bytecopy_burst_picobin, sizeof(picorv_bytecopy_burst_picobin), 1},
#endif
#endif /*BYTE_COPY*/
};

//Struct hold memory type attributes.
typedef struct {
    const char* name;
    char* ptr;
} MemType;

//Source memory type test vector.
MemType srcMems[] = {
#ifdef LOCAL_MEM_COPY
    {
        "LOCAL",
        (unsigned char*)srcBufLocal,
    },
#endif
#ifdef EXT_MEM0_COPY
    {
        "EXT_MEM_0",
        (unsigned char*)MAIN_RAM_BASE,
    },
#endif
#ifdef VRAM_MEM_COPY
    {
        "VRAM",
        (unsigned char*)VERA_VRAM_BASE,
    },
#endif
};

//Destination memory type test vector.
MemType dstMems[] = {
#ifdef LOCAL_MEM_COPY
    {
        "LOCAL",
        (unsigned char*)dstBufLocal,
    },
#endif
#ifdef EXT_MEM0_COPY
    {
        "EXT_MEM_0",
        (unsigned char*)(MAIN_RAM_BASE+64*4),
    },
#endif
#ifdef VRAM_MEM_COPY
    {
        "VRAM",
        (unsigned char*)(VERA_VRAM_BASE+64*4),
    },
#endif
};

//Source pointer offsets test vector.
unsigned srcOffsets[] = {
#ifdef OFFSET_0
    0,
#endif
#ifdef OFFSET_1
    1,
#endif
#ifdef OFFSET_2
    2,
#endif
#ifdef OFFSET_3
    3,
#endif
};

//Destination pointer offsets test vector.
unsigned dstOffsets[] = {
#ifdef OFFSET_0
    0,
#endif
#ifdef OFFSET_1
    1,
#endif
#ifdef OFFSET_2
    2,
#endif
#ifdef OFFSET_3
    3,
#endif
};

//Number of elements test vector.
unsigned numElems[] = {
#ifdef ONE_SIZE_ONLY
    32
#else
    1, 5, 16, 31, 32
    //1, 3, 4, 5, 8, 12, 13, 15, 16, 24, 25, 28, 29, 31, 32
#endif
};

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

//Instead of polling for DMA copy-complete status, let's use interrupts.
//This way, DMA interrupt handling also gets some test coverage.
volatile int dma_irq_fired = 0;

void _dmac_irq_handler(void) {
  /*Acknowledge IRQ at the source*/
  picorv_sys_reg_wr(PICORV_SYS_REG_IRQ_OUT, 1);
  dma_irq_fired = 1;
}

//Parameterized dma test function. Returns 1 if success, 0 if failed.
int dmaTest(unsigned char* srcBase, unsigned srcOffset,
            unsigned char* dstBase, unsigned dstOffset,
            unsigned numElems, unsigned elemSize) {
    unsigned char* srcPtr = srcBase + srcOffset*elemSize;
    //Offset destination pointer deeper into its buffer so we have room to check for out-of-bounds accesses before
    //the destination start pointer.
    unsigned char* dstPtr = dstBase + 8*elemSize + dstOffset*elemSize;

    //Fill source buffer with some random data and destination with 0x55s
    for (int ii=0; ii<64*4; ii++) {
        srcBase[ii] = (unsigned char)rand();
        dstBase[ii] = 0x55;
    }

#ifdef DETAILED_LOGS
    printf("Configuring DMA request...\n");
    printf("numElems = %d, srcAddr = 0x%x, dstAddr = 0x%x\n", numElems, (unsigned)srcPtr, (unsigned)dstPtr);
#endif

    picorv_hir_reg_wr(PICORV_HIR_REG_SRC, (unsigned)srcPtr);
    picorv_hir_reg_wr(PICORV_HIR_REG_DST, (unsigned)dstPtr);
    picorv_hir_reg_wr(PICORV_HIR_REG_NUM_ELEMS, numElems);

#ifdef DETAILED_LOGS
    printf("Kicking off DMA...\n");
#endif
    picorv_hir_reg_wr(PICORV_HIR_REG_CTRL_STAT, DMA_START);

#ifdef DETAILED_LOGS
    printf("Waiting for completion...\n");
#endif
    /*Of course, we could just poll the DMA status, but we want to test IRQ generation as well.*/
    while (!dma_irq_fired);
    dma_irq_fired = 0;
    /*Assert that DMA is no longer busy*/
    assert(picorv_hir_reg_rd(PICORV_HIR_REG_CTRL_STAT) == 0);

#ifdef DETAILED_LOGS
    printf("Checking result...\n");
#endif
    if (!memcmp(srcPtr, dstPtr, numElems*elemSize)) {
#ifdef DETAILED_LOGS
        printf("Memcmp successful.\n");
#endif
        int oob=0; /*out-of-bounds access detected flag.*/

        //check bytes before the destination start pointer.
        for (int idx=-1; idx>-8; idx--) {
            if (dstPtr[idx] != 0x55) {
                printf("Out of bounds write at idx %d\n", idx);
                oob=1;
            }
        }

        //check bytes after the destination end pointer.
        for (int idx = numElems*elemSize; idx<numElems*elemSize+8; idx++) {
            if (dstPtr[idx] != 0x55) {
                printf("Out of bounds write at idx %d\n", idx);
                oob=1;
            }
        }

        if (oob) {
            printf("numElems = %d, srcAddr = 0x%x, dstAddr = 0x%x\n", numElems, (unsigned)srcPtr, (unsigned)dstPtr);
            return 0;
        }
        else {
            return 1;
        }
    }
    else {
        printf("Memcmp failed.\n");
        printf("numElems = %d, srcAddr = 0x%x, dstAddr = 0x%x\n", numElems, (unsigned)srcPtr, (unsigned)dstPtr);

        for (int ii=0; ii<numElems*elemSize; ii++) {
            printf("[%d] 0x%x vs. 0x%x\n", ii, srcPtr[ii], dstPtr[ii]);
        }

        return 0;
    }
}

int main(void) {
  //Switches
  gpio_init(&gpio, (volatile void *) GPIO_BASE);
  gpio_set_direction(&gpio, 0x0000000F);

  /*sdram_init() is provided by the Litex code base.*/
  if (sdram_init()) {
    printf("SDRAM init OK.\n");
  }
  else {
    printf("SDRAM init failed!\n");
    while(1);
  }

  printf("Enabling IRQs\n");
  enable_global_irq(); //Enable the global IRQ at CPU level.
  enable_irq(IRQ_ID_DMAC); //Enable the DMA IRQ at CPU level.
  //Note that there's no IRQ ENABLE register at DMA core level, i.e. at DMA core level,
  //IRQs are always enabled. This behaviour can be customized by defining ISR and IEN
  //Host Interface (HIR) registers.

  int numFailedTests = 0;

  //Nested loop of test vectors.

  //PicoRV program
  for (int progIdx = 0; progIdx < sizeof(programs)/sizeof(Program); progIdx++) {
    Program *pp = &programs[progIdx];

    printf("Load PicoRV Program %s\n", pp->name);

    picorv_load_program(pp->progDat, pp->progLen);

    printf("Taking PicoRV out of reset...\n");
    picorv_sys_reg_wr(PICORV_SYS_REG_CTRL, 1);

    unsigned elemSize = pp->elemSize;

    //Source memory type.
    for (int srcMemIdx = 0; srcMemIdx < sizeof(srcMems)/sizeof(MemType); srcMemIdx++) {
        MemType *srcMemType = &srcMems[srcMemIdx];

        printf("Src Mem Type %s, addr=0x%x\n", srcMemType->name, srcMemType->ptr);

        unsigned char *srcMemBase = srcMemType->ptr;

        //Source offset.
        for (int srcOffsetIdx=0; srcOffsetIdx < sizeof(srcOffsets)/sizeof(unsigned); srcOffsetIdx++) {
            unsigned srcOffset = srcOffsets[srcOffsetIdx];
#ifdef DETAILED_LOGS
            printf("srcOffset = %d\n", srcOffset);
#endif
            //Destination memory type.
            for (int dstMemIdx = 0; dstMemIdx < sizeof(dstMems)/sizeof(MemType); dstMemIdx++) {
                MemType *dstMemType = &dstMems[dstMemIdx];

                printf("Dst Mem Type %s, addr=0x%x\n", dstMemType->name, dstMemType->ptr);

                unsigned char *dstMemBase = dstMemType->ptr;

                //Destination offset.
                for (int dstOffsetIdx=0; dstOffsetIdx < sizeof(dstOffsets)/sizeof(unsigned); dstOffsetIdx++) {
                    unsigned dstOffset = dstOffsets[dstOffsetIdx];
#ifdef DETAILED_LOGS
                    printf("dstOffset = %d\n", dstOffset);
#endif
                    //Number of elements to copy.
                    for (int numElemsIdx=0; numElemsIdx < sizeof(numElems)/sizeof(unsigned); numElemsIdx++) {
                        unsigned nElems = numElems[numElemsIdx];
#ifdef DETAILED_LOGS
                        printf("numElems = %d\n", nElems);
#endif
                        if (dmaTest(srcMemBase, srcOffset,
                                    dstMemBase, dstOffset, nElems, elemSize)) {
#ifdef DETAILED_LOGS
                            printf("Test Successful.\n");
#endif
                        }
                        else {
                            printf("Test Failed.\n");
                            ++numFailedTests;
#ifdef STOP_ON_FAIL
                            return 0;
#endif
                        }
                    }
                }
            }
        }
    }

    printf("Putting PicoRV back into reset...\n");
      picorv_sys_reg_wr(PICORV_SYS_REG_CTRL, 0);
  }

  if (numFailedTests == 0) {
    printf("All tests passed.\n");
  }
  else {
    printf("%d tests failed.\n", numFailedTests);
  }

  return 0;
}
