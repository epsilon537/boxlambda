////////////////////////////////////////////////////////////////////////////////
//
// Epsilon/BoxLambda: This is a simplified version of the flash driver from
// ZipCPU's qpspiflash repository:
//
// https://github.com/ZipCPU/qspiflash
//
// Purpose:    Flash driver.  Encapsulates writing, both erasing sectors and
//        the programming pages, to the flash device.
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
#ifndef    FLASHDRVR_H
#define    FLASHDRVR_H

#include "memmap.h"

class    FLASHDRVR {
private:
    bool    m_debug;
    uint32_t    m_id; // ID of the flash device
    char    *m_sbuf;

    void    flwait(void);
public:
    FLASHDRVR();
    bool    erase_sector(const uint32_t sector, const bool verify_erase=true);
    bool    page_program(const uint32_t addr, const uint32_t len,
            const char *data, const bool verify_write=true);

   /* Write the given block of data to the given flash memory address.
    * Note that flash memory is divided into 4KB segments. Writing to a segment
    * will destroy all previous contents of that segment.
    */
    bool    write(const uint32_t addr, const uint32_t len,
            const char *data, const bool verify=false);

    uint32_t    flashid(void);
};

#endif

