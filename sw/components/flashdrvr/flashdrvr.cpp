////////////////////////////////////////////////////////////////////////////////
//
// Epsilon/BoxLambda: This is a simplified version of the flash driver from
// ZipCPU's qpspiflash repository:
//
// Purpose:    Flash driver.  Encapsulates the erasing and programming (i.e.
//        writing) necessary to set the values in a flash device.
//
// Original Creator:    Dan Gisselquist, Ph.D.
//                      Gisselquist Technology, LLC
//
////////////////////////////////////////////////////////////////////////////////
// }}}
// Copyright (C) 2015-2021, Gisselquist Technology, LLC
// {{{
// This file is part of the set of Wishbone controlled SPI flash controllers
// project
//
// The Wishbone SPI flash controller project is free software (firmware):
// you can redistribute it and/or modify it under the terms of the GNU Lesser
// General Public License as published by the Free Software Foundation, either
// version 3 of the License, or (at your option) any later version.
//
// The Wishbone SPI flash controller project is distributed in the hope
// that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTIBILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  (It's in the $(ROOT)/doc directory.  Run make
// with no target there if the PDF file isn't present.)  If not, see
// <http://www.gnu.org/licenses/> for a copy.
// }}}
// License:    LGPL, v3, as defined and found on www.gnu.org,
// {{{
//        http://www.gnu.org/licenses/lgpl.html
//
////////////////////////////////////////////////////////////////////////////////
//
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <strings.h>
#include <assert.h>

#include "flashdrvr.h"
#include "spiflash_regs.h"
#include "byteswap.h"

// Flash control constants
#define    SZPAGEB        256
#define    PGLENB        256
#define    SZPAGEW        64
#define    PGLENW        64
#define    NPAGES        256
#define    SECTORSZB    (NPAGES * SZPAGEB)    // In bytes, not words!!
#define    SECTORSZW    (NPAGES * SZPAGEW)    // In words
#define    NSECTORS    64
#define    SECTOROF(A)    ((A) & (-1<<16))
#define    SUBSECTOROF(A)    ((A) & (-1<<12))
#define    PAGEOF(A)    ((A) & (-1<<8))

#ifndef    FLASH_UNKNOWN
#define    FLASH_UNKNOWN    0
#endif

#define    CFG_USER_CS_n    (1<<8)

static const unsigned    F_RESET = (0x0ff),
            F_EMPTY = (0x000),
            F_WRR   = (0x001),
            F_PP    = (0x002),
            F_QPP   = (0x032),
            F_READ  = (0x003),
            F_WRDI  = (0x004),
            F_RDSR1 = (0x005),
            F_WREN  = (0x006),
            F_MFRID = (0x09f),
            F_SE    = (0x0d8),
            F_END   = (CFG_USER_CS_n);

// Read a series of values from values from a block of memory
//    a is the address of the value to be read as it exists on the
//        wishbone bus within the FPGA.
//    len is the number of words to read
//    buf is a pointer to a place to store the words once read.
static inline void readi(uint32_t a, const int len, uint32_t *buf) {
    for(int i=0; i<len; i++)
        buf[i] = *(volatile uint32_t*)(a+i);
}

//BoxLambda: define a default constructor referencing the singleton devbus object.
FLASHDRVR::FLASHDRVR() : m_debug(false), m_id(FLASH_UNKNOWN) {
  //BoxLambda: Allocating an sbuf on the heap. This object is too big to fit in
  //IMEM or worse, on the stack.
  m_sbuf = (char*)malloc(SECTORSZB);
  assert(m_sbuf);
}

unsigned FLASHDRVR::flashid(void) {
    unsigned    r;

    if (m_id != FLASH_UNKNOWN)
        return m_id;

    SPIFLASH->CTRL = F_MFRID;
    SPIFLASH->CTRL = 0x00;
    r = SPIFLASH->CTRL & 0x0ff;
    SPIFLASH->CTRL = 0x00;
    r = (r<<8) | (SPIFLASH->CTRL & 0x0ff);
    SPIFLASH->CTRL = 0x00;
    r = (r<<8) | (SPIFLASH->CTRL & 0x0ff);
    SPIFLASH->CTRL = 0x00;
    r = (r<<8) | (SPIFLASH->CTRL & 0x0ff);
    m_id = r;

//  printf("flash ID returning %08x\n", m_id);
    return m_id;
}

void    FLASHDRVR::flwait(void) {
    const    int    WIP = 1;    // Write in progress bit
    uint32_t    sr;

    SPIFLASH->CTRL = F_END;
    SPIFLASH->CTRL = F_RDSR1;
    do {
        SPIFLASH->CTRL = F_EMPTY;
        sr = SPIFLASH->CTRL;
    } while(sr&WIP);
    SPIFLASH->CTRL = F_END;
}

bool    FLASHDRVR::erase_sector(const unsigned sector, const bool verify_erase) {
    unsigned    flashaddr = sector & 0x0ffffff;

    // Write enable
    SPIFLASH->CTRL = F_END;
    SPIFLASH->CTRL = F_WREN;
    SPIFLASH->CTRL = F_END;

    uint32_t    page[SZPAGEW];

    // printf("EREG before   : %08x\n", m_fpga->readio(R_QSPI_EREG));
    printf("Erasing sector: %06x\n", flashaddr);

    SPIFLASH->CTRL = F_SE;
    SPIFLASH->CTRL = ((flashaddr>>16)&0x0ff);
    SPIFLASH->CTRL = ((flashaddr>> 8)&0x0ff);
    SPIFLASH->CTRL = ((flashaddr    )&0x0ff);
    SPIFLASH->CTRL = F_END;

    // Wait for the erase to complete
    flwait();

    // Now, let's verify that we erased the sector properly
    if (verify_erase) {
        if (m_debug)
            printf("Verifying the erase\n");
        for(int i=0; i<NPAGES; i++) {
            // printf("READI[%08x + %04x]\n", FLASHBASE+flashaddr+i*SZPAGEB, SZPAGEW);
            readi(FLASHBASE+flashaddr+i*SZPAGEB, SZPAGEW, page);
            for(int j=0; j<SZPAGEW; j++)
                if (page[j] != 0xffffffff) {
                    unsigned rdaddr = FLASHBASE+flashaddr+i*SZPAGEB;

                    if (m_debug)
                        printf("FLASH[%07x] = %08x, not 0xffffffff as desired (%06x + %d)\n",
                            FLASHBASE+flashaddr+i*SZPAGEB+(j<<2),
                            page[j], rdaddr,(j<<2));
                    return false;
                }
        }
        if (m_debug)
            printf("Erase verified\n");
    }

    return true;
}

bool    FLASHDRVR::page_program(const unsigned addr, const unsigned len,
        const char *data, const bool verify_write) {
    uint32_t    buf[SZPAGEW], bswapd[SZPAGEW];
    unsigned    flashaddr = addr & 0x0ffffff;

    assert(len > 0);
    assert(len <= PGLENB);
    assert(PAGEOF(addr)==PAGEOF(addr+len-1));

    if (len <= 0)
        return true;

    bool    empty_page = true;
    for(unsigned i=0; i<len; i+=4) {
        uint32_t v;
        v = buildword((const unsigned char *)&data[i]);
        bswapd[(i>>2)] = v;
        if (v != 0xffffffff)
            empty_page = false;
    }

    if (!empty_page) {
        // Write enable
        SPIFLASH->CTRL = F_END;
        SPIFLASH->CTRL = F_WREN;
        SPIFLASH->CTRL = F_END;

        //
        // Write the page
        //

        // Issue the page program command
        //
        // Our interface will limit us, so there's no reason to use
        // QUAD page programming here
        // if (F_QPP) {} else
        SPIFLASH->CTRL = F_PP;
        // The address
        SPIFLASH->CTRL = ((flashaddr>>16)&0x0ff);
        SPIFLASH->CTRL = ((flashaddr>> 8)&0x0ff);
        SPIFLASH->CTRL = ((flashaddr    )&0x0ff);

        // Write the page data itself
        for(unsigned i=0; i<len; i++)
            SPIFLASH->CTRL =
                (data[i] & 0x0ff);
        SPIFLASH->CTRL = F_END;

        printf("Writing page: 0x%08x - 0x%08x", addr, addr+len-1);
        if ((m_debug)&&(verify_write))
            fflush(stdout);
        else
            printf("\n");

        flwait();
    }

    if (verify_write) {

        // printf("Attempting to verify page\n");
        // NOW VERIFY THE PAGE
        readi(addr, len>>2, buf);
        for(unsigned i=0; i<(len>>2); i++) {
            if (buf[i] != bswapd[i]) {
                printf("\nVERIFY FAILS[%d]: %08x\n", i, (i<<2)+addr);
                printf("\t(Flash[%d]) %08x != %08x (Goal[%08x])\n",
                    (i<<2), buf[i], bswapd[i], (i<<2)+addr);
                return false;
            }
        } if (m_debug)
            printf(" -- Successfully verified\n");
    } return true;
}

bool    FLASHDRVR::write(const unsigned addr, const unsigned len,
        const char *data, const bool verify) {

    assert(addr >= FLASHBASE);
    assert(addr+len <= FLASHBASE + FLASHLEN);

    // Work through this one sector at a time.
    // If this buffer is equal to the sector value(s), go on
    // If not, erase the sector

    for(unsigned s=SECTOROF(addr); s<SECTOROF(addr+len+SECTORSZB-1);
            s+=SECTORSZB) {
        // Do we need to erase?
        bool    need_erase = false, need_program = false;
        unsigned newv = 0; // (s<addr)?addr:s;
        {
            char *sbuf = m_sbuf;
            const char *dp;    // pointer to our "desired" buffer
            unsigned    base,ln;

            base = (addr>s)?addr:s;
            ln=((addr+len>s+SECTORSZB)?(s+SECTORSZB):(addr+len))-base;
            readi(base, ln>>2, (uint32_t*)sbuf);
            byteswapbuf(ln>>2, (uint32_t *)sbuf);

            dp = &data[base-addr];

            for(unsigned i=0; i<ln; i++) {
                if ((sbuf[i]&dp[i]) != dp[i]) {
                    if (m_debug) {
                        printf("\nNEED-ERASE @0x%08x ... %08x != %08x (Goal)\n",
                            i+base-addr, sbuf[i], dp[i]);
                    }
                    need_erase = true;
                    newv = (i&-4)+base;
                    break;
                } else if ((sbuf[i] != dp[i])&&(newv == 0))
                    newv = (i&-4)+base;
            }
        }

        if (newv == 0)
            continue; // This sector already matches

        // Erase the sector if necessary
        if (!need_erase) {
            if (m_debug) printf("NO ERASE NEEDED\n");
        } else {
            printf("ERASING SECTOR: %08x\n", s);
            if (!erase_sector(s, verify)) {
                printf("SECTOR ERASE FAILED!\n");
                return false;
            } newv = (s<addr) ? addr : s;
        }

        // Now walk through all of our pages in this sector and write
        // to them.
        for(unsigned p=newv; (p<s+SECTORSZB)&&(p<addr+len); p=PAGEOF(p+PGLENB)) {
            unsigned start = p, len = addr+len-start;

            // BUT! if we cross page boundaries, we need to clip
            // our results to the page boundary
            if (PAGEOF(start+len-1)!=PAGEOF(start))
                len = PAGEOF(start+PGLENB)-start;
            if (!page_program(start, len, &data[p-addr], verify)) {
                printf("WRITE-PAGE FAILED!\n");
                return false;
            }
        } if ((need_erase)||(need_program))
            printf("Sector 0x%08x: DONE%15s\n", s, "");
    }

    SPIFLASH->CTRL = F_WRDI;
    SPIFLASH->CTRL = F_END;

    return true;
}
