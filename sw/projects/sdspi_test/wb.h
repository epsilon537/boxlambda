#ifndef WB_H
#define WB_H

#include <stdio.h>

static unsigned wb_read(unsigned a) {
    return *(volatile unsigned *)(a);
}

static void wb_write(unsigned a, unsigned v) {
    printf("WB-WRITEM(%08x) <= %08x\n", a, v);
    *(volatile unsigned *)(a) = v;
}

static void	wb_read_buf(unsigned a, int len, unsigned *buf, const int inc) {
    printf("WB-READM(%08x, %d)\n", a, len);

    for (int cnt=0; cnt<len; cnt++) {
        buf[cnt] = *(volatile unsigned *)(a);
        a+=inc*4;
    }
}

static void wb_write_buf(unsigned a, unsigned int len, unsigned *buf, const int inc) {
    printf("WB-WRITEM(%08x, %d, ...)\n", a, len);
    
    for (int cnt=0; cnt<len; cnt++) {
        *(volatile unsigned *)(a) = buf[cnt];
        a+=inc*4;
    }
}

#endif //WB_H
