/************************************************************************/
/*                                                                      */
/* BoxLambda: this module is based on Digilent's RTCCI2C library        */
/* for Arduino:                                                         */
/*                                                                      */
/* https://digilent.com/reference/_media/reference/pmod/pmodrtcc/rtcci2c.zip */
/*                                                                      */
/*  RTCCI2C.h  --  Declaration for RTCCI2C library                      */
/*                                                                      */
/************************************************************************/
/*  Original Author:    Michelle Yu                                     */
/*  Copyright 2011, Digilent Inc.                                       */
/************************************************************************/
/*  File Description:                                                   */
/*    This file declares functions for RTCCI2C                          */
/*                                                                      */
/************************************************************************/
/*  Revision History:                                                   */
/*                                                                      */
/*  13/07/2024(Epsilon537): Modifications for BoxLambda.                */
/*  11/09/2011(MichelleY): created                                      */
/*                                                                      */
/************************************************************************/
#if !defined(RTCCI2C_H)
#define RTCCI2C_H

/* ------------------------------------------------------------ */
/*        Include File Definitions                              */
/* ------------------------------------------------------------ */
#include <stdint.h>

/* ------------------------------------------------------------ */
/*          Procedure Declarations                              */
/* ------------------------------------------------------------ */
//available source/destination
#define RTCC_RTCC    0x01    // real- time clock
#define RTCC_ALM0    0x02    // alarm 0
#define RTCC_ALM1    0x03    // alarm 1
#define RTCC_PWRD    0x04    // power-down time-stamp
#define RTCC_PWRU    0x05    // power-up time-stamp

//alarm configuration bits
#define RTCC_ALM_POL 0x80
#define RTCC_ALMC2  0x40
#define RTCC_ALMC1  0x20
#define RTCC_ALMC0  0x10

//AM/PM
#define RTCC_AM 0
#define RTCC_PM 1

//clock functions
void startClock(void);
void stopClock(void);

//backup battery functions
void enableVbat(void); //Enables the backup battery
void disableVbat(void); //Disables the backup battery

//get functions
uint8_t getSec (void);           //src = RTCC
uint8_t getMin (uint8_t src);    //src = RTCC_RTCC/RTCC_PWRD/RTCC_PWRU
uint8_t getHour (uint8_t src);   //src = RTCC_RTCC/RTCC_PWRD/RTCC_PWRU
uint8_t getAmPm (uint8_t src);   //src = RTCC_RTCC/RTCC_PWRD/RTCC_PWRU
uint8_t getDay (uint8_t src);    //src = RTCC_RTCC/RTCC_PWRD/RTCC_PWRU
uint8_t getDate (uint8_t src);   //src = RTCC_RTCC/RTCC_PWRD/RTCC_PWRU
uint8_t getMonth (uint8_t src);  //src = RTCC_RTCC/RTCC_PWRD/RTCC_PWRU
uint8_t getYear ();              //src = RTCC_RTCC

//set functions
//dest = RTCC_RTCC
void setSec(uint8_t value);
void setMin(uint8_t value);
void setHour(uint8_t value, uint8_t ampm);
void setHour(uint8_t value);
void setDay(uint8_t value);
void setDate(uint8_t value);
void setMonth(uint8_t value);
void setYear(uint8_t value);

#endif




