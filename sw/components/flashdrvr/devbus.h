////////////////////////////////////////////////////////////////////////////////
//
// Filename:    devbus.h
//
// Project:    OpenArty, an entirely open SoC based upon the Arty platform
//
// Purpose:    The purpose of this file is to document an interface which
//        any device with a bus, whether it be implemented over a UART,
//    an ethernet, or a PCI express bus, must implement.  This describes
//    only an interface, and not how that interface is to be accomplished.
//
//    The neat part of this interface is that, if programs are designed to
//    work with it, than the implementation details may be changed later
//    and any program that once worked with the interface should be able
//    to continue to do so.  (i.e., switch from a UART controlled bus to a
//    PCI express controlled bus, with minimal change to the software of
//    interest.)
//
//
// Creator:    Dan Gisselquist, Ph.D.
//        Gisselquist Technology, LLC
//
////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2015-2019, Gisselquist Technology, LLC
//
// This program is free software (firmware): you can redistribute it and/or
// modify it under the terms of  the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program.  (It's in the $(ROOT)/doc directory.  Run make with no
// target there if the PDF file isn't present.)  If not, see
// <http://www.gnu.org/licenses/> for a copy.
//
// License:    GPL, v3, as defined and found on www.gnu.org,
//        http://www.gnu.org/licenses/gpl.html
//
//
////////////////////////////////////////////////////////////////////////////////
//
//
#ifndef    DEVBUS_H
#define    DEVBUS_H

#include <stdio.h>
#include <unistd.h>

typedef    unsigned int    uint32;

class    BUSERR {
public:
    uint32 addr;
    BUSERR(const uint32 a) : addr(a) {};
};

class    DEVBUS {
public:
    typedef    uint32    BUSW;

    // Write a single value to a single address
    //    a is the address of the value to be read as it exists on the
    //        wishbone bus within the FPGA.
    //    v is the singular value to write to this address
    inline    void    writeio(const BUSW a, const BUSW v) {
        *(volatile BUSW*)a = v;
    }

    // Read a single value to a single address
    //    a is the address of the value to be read as it exists on the
    //        wishbone bus within the FPGA.
    //    This function returns the value read from the device wishbone
    //        at address a.
    inline    BUSW    readio(const BUSW a) {
        return *(volatile BUSW*)a;
    }

    // Read a series of values from values from a block of memory
    //    a is the address of the value to be read as it exists on the
    //        wishbone bus within the FPGA.
    //    len is the number of words to read
    //    buf is a pointer to a place to store the words once read.
    inline    void    readi(const BUSW a, const int len, BUSW *buf) {
        for(int i=0; i<len; i++)
            buf[i] = readio(a+i);
    }

    // Read a series of values from the same address in memory.  This
    // call is identical to readi, save that the address is not incremented
    // from one read to the next.
    inline    void    readz(const BUSW a, const int len, BUSW *buf) {
        for(int i=0; i<len; i++)
            buf[i] = readio(a);
    }

    // Write a series of values into a block of memory on the FPGA
    //    a is the address of the value to be written as it exists on the
    //        wishbone bus within the FPGA.
    //    len is the number of words to write
    //    buf is a pointer to a place to from whence to grab the data
    //        to be written.
    inline    void    writei(const BUSW a, const int len, const BUSW *buf) {
        for(int i=0; i<len; i++)
            writeio(a+i, buf[i]);
    }

    // Write a series of values into the same address on the FPGA bus.  This
    // call is identical to writei, save that the address is not incremented
    // from one write to the next.
    inline    void    writez(const BUSW a, const int len, const BUSW *buf) {
        for(int i=0; i<len; i++)
            writeio(a, buf[i]);
    }
};

#endif
