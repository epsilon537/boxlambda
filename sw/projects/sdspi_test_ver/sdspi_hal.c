#include "sdspi_hal.h"
#include "wb.h"
//#include <assert.h>
#include <stdio.h>

unsigned sdspi_read_aux(void) {
    wb_write(SDSPI_CMD_ADDR, READAUX);
    return wb_read(SDSPI_DATA_ADDR);
}

unsigned sdspi_set_aux(unsigned aux) {
    wb_write(SDSPI_DATA_ADDR, aux);
    wb_write(SDSPI_CMD_ADDR, SETAUX);
    return wb_read(SDSPI_DATA_ADDR);
}

void sdspi_wait_while_busy(void) {
    while(wb_read(SDSPI_CMD_ADDR) & SDSPI_BUSY);
}

unsigned sdspi_sdcmd(int cmd, unsigned arg) {
    wb_write(SDSPI_DATA_ADDR, arg);
    wb_write(SDSPI_CMD_ADDR, cmd);

    sdspi_wait_while_busy();
    return wb_read(SDSPI_CMD_ADDR);
}

unsigned sdspi_read(int cmd, unsigned arg, int ln, unsigned *data) {
    unsigned	fifo_addr;
    unsigned	lglen, r;

    if (cmd & SDSPI_FIFO_ID)
        fifo_addr = SDSPI_FIFO_B;
    else
        fifo_addr = SDSPI_FIFO_A;

    //assert((cmd & SDSPI_WRITEOP) == SDSPI_FIFO_OP);

    for(lglen = 4; (1<<lglen) < ln; lglen++)
        ;
    sdspi_set_aux(lglen << 16);

    printf("LGLEN = %d, LN = %d\n", lglen, ln);
    //assert((1<<lglen) == ln);

    wb_write(SDSPI_DATA_ADDR, arg);
    wb_write(SDSPI_CMD_ADDR, cmd);
    sdspi_wait_while_busy();

    r = wb_read(SDSPI_CMD_ADDR);
    wb_read_buf(fifo_addr, (1<<(lglen-2)), data, 0);

    return	r;
}

unsigned sdspi_write(int cmd, unsigned arg, int ln, unsigned *data) {
    unsigned	fifo_addr;
    unsigned	lglen;

    if (cmd & SDSPI_FIFO_ID)
        fifo_addr = SDSPI_FIFO_B;
    else
        fifo_addr = SDSPI_FIFO_A;

    //assert((cmd & SDSPI_WRITEOP) == SDSPI_WRITEOP);

    for(lglen = 4; (1<<lglen) < ln; lglen++)
        ;
    sdspi_set_aux(lglen << 16);

    //assert((1<<lglen) == ln);

    wb_write_buf(fifo_addr, (1<<(lglen-2)), data, 0);

    wb_write(SDSPI_DATA_ADDR, arg);
    wb_write(SDSPI_CMD_ADDR, cmd);
    sdspi_wait_while_busy();
    return	wb_read(SDSPI_CMD_ADDR);
}

////////////////////////////////////////////////////////////////////////
//
unsigned sdspi_read_ocr(void) {
    unsigned	r;
    r = sdspi_sdcmd(SDSPI_READREG | SDSPI_CLEARERR | SDSPI_CMD + 58,0);

    r = wb_read(SDSPI_DATA_ADDR);
    printf("R   : 0x%08x\n", r);
    return r;
}

unsigned sdspi_read_csd(unsigned *data) {
    unsigned	r;
    r = sdspi_read(SDSPI_CLEARERR|SDSPI_FIFO_OP|SDSPI_CMD+9, 0,
            16, data);
    return r;
}

unsigned sdspi_read_cid(unsigned *data) {
    unsigned	r;
    r = sdspi_read(SDSPI_CLEARERR|SDSPI_FIFO_OP|SDSPI_CMD+10, 0,
            16, data);
    return r;
}
