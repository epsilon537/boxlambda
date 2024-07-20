/************************************************************************/
/*                                                                      */
/*  RTCCI2C.c  --  Definition for RTCCI2C library                       */
/*                                                                      */
/************************************************************************/
/*  Author:    Michelle Yu / Digilent, Epsilson537/BoxLambda            */
/*  Copyright 2011, Digilent Inc.                                       */
/************************************************************************/
/*  File Description:                                                   */
/*    This file defines functions for RTCCI2C                           */
/*                                                                      */
/************************************************************************/
/*  Revision History:                                                   */
/*                                                                      */
/*  11/09/2011(MichelleY): created                                      */
/*                                                                      */
/************************************************************************/


/* ------------------------------------------------------------ */
/*        Include File Definitions                              */
/* ------------------------------------------------------------ */
#include "rtcci2c.h"
#include "i2c_master_hal.h"
#include <assert.h>

#define I2C_SLAVE_ADDRESS 0x6F

/* ------------------------------------------------------------ */
/*        Procedure Definitions                                 */
/* ------------------------------------------------------------ */

//clock functions
/* ------------------------------------------------------------------- */
/** void startClock(void)
**
**  Parameters:
**    None
**
**
**  Return Value:
**    None
**
**
**  Description:
**    This function starts the RTCC_RTCC
*/
void startClock(void)
{
  uint8_t temp = 0;

  //temp = 0x08;
  //i2c_master_write_bytes(&temp, I2C_SLAVE_ADDRESS, 0x7, 1);

  i2c_master_read_bytes(&temp, I2C_SLAVE_ADDRESS, 0, 1);
  //The 7th bit of the RTCC_RTCC second register controls the oscillator

  //set the 7th bit to start the clock
  temp = (temp | 0x80);

  i2c_master_write_bytes(&temp, I2C_SLAVE_ADDRESS, 0, 1);
}

/* ------------------------------------------------------------------- */
/** void stopClock(void)
**
**  Parameters:
**    None
**
**
**  Return Value:
**    None
**
**
**  Description:
**    This function stops the RTCC_RTCC
*/
void stopClock(void)
{
  uint8_t temp = 0;

  i2c_master_read_bytes(&temp, I2C_SLAVE_ADDRESS, 0, 1);
  //The 7th bit of the RTCC_RTCC second register controls the oscillator

  //clear the 7th bit to stop the clock
  temp = (temp & 0x80);

  i2c_master_write_bytes(&temp, I2C_SLAVE_ADDRESS, 0, 1);
}

//backup battery functions
/* ------------------------------------------------------------------- */
/** void enableVbat(void)
**
**  Parameters:
**    None
**
**  Return Value:
**    None
**
**  Description:
**    This function enables backup battery mode
*/
void enableVbat(void)
{
  uint8_t temp = 0;

  i2c_master_read_bytes(&temp, I2C_SLAVE_ADDRESS, 0x3, 1);
  //The 3rd bit of the RTCC_RTCC day register controls VBATEN

  //set 3rd bit to enable backup battery mode
  temp = (temp | 0x08);

  i2c_master_write_bytes(&temp, I2C_SLAVE_ADDRESS, 0x3, 1);
}

/* ------------------------------------------------------------------- */
/** void disableVbat(void)
**
**  Parameters:
**    None
**
**  Return Value:
**    None
**
**  Description:
**    This function disables backup battery mode
*/
void disableVbat(void)
{
  uint8_t temp = 0;

  i2c_master_read_bytes(&temp, I2C_SLAVE_ADDRESS, 0x3, 1);
  //The 3rd bit of the RTCC_RTCC day register controls VBATEN

  //clear 3rd bit to disable backup battery mode
  temp = (temp & 0x37);

  i2c_master_write_bytes(&temp, I2C_SLAVE_ADDRESS, 0x3, 1);
}

//get functions
/* ------------------------------------------------------------------- */
/** uint8_t getSec(void)
 **
 **  Return Value:
 **    uint8_t - second in hexadecimal
 **
 **  Description:
 **    This function returns the second of the source
 */
uint8_t getSec(void)
{
  uint8_t bSecBuffer = 0;

  i2c_master_read_bytes(&bSecBuffer, I2C_SLAVE_ADDRESS, 0x0, 1);

  // return valid bits
  return (bSecBuffer /*& 0x7F*/);
}

/* ------------------------------------------------------------------- */
/** uint8_t getMin(uint8_t src)
**
**  Parameters:
**    src - RTCC_RTCC - real-time clock
**          RTCC_PWRD - Power-down time-stamp
**          RTCC_PWRU - Power-up time-stamp
**
**  Return Value:
**    uint8_t - minute in hexadecimal
**
**  Description:
**    This function returns the minute of the source
*/
uint8_t getMin(uint8_t src)
{
  uint8_t bMinBuffer = 0;
  unsigned bMin;

  //Set address of the minute register depending on the source
  switch(src)
  {
    case RTCC_RTCC:
      bMin = 0x01;
      break;
    case RTCC_PWRD:
      bMin = 0x18;
      break;
    case RTCC_PWRU:
      bMin = 0x1C;
      break;
    default:
      return 0xFF; //ERROR Invalid Source
  }

  i2c_master_read_bytes(&bMinBuffer, I2C_SLAVE_ADDRESS, bMin, 1);

  // return valid bits
  return (bMinBuffer & 0x7F);
}
/* ------------------------------------------------------------------- */
/** uint8_t getHour(uint8_t src)
**
**  Parameters:
**    src - RTCC_RTCC - real-time clock
**          RTCC_PWRD - Power-down time-stamp
**          RTCC_PWRU - Power-up time-stamp
**
**  Return Value:
**    uint8_t - hour in hexadecimal
**
**  Description:
**    This function returns the hour of the source
*/
uint8_t getHour(uint8_t src)
{
  uint8_t bHourBuffer = 0;
  unsigned bHour;
  //Set address of the hour register depending on the source
  switch(src)
  {
    case RTCC_RTCC:
      bHour = 0x02;
      break;
    case RTCC_PWRD:
      bHour = 0x19;
      break;
    case RTCC_PWRU:
      bHour = 0x1D;
      break;
    default:
      return 0xFF; //ERROR Invalid Source
  }

  i2c_master_read_bytes(&bHourBuffer, I2C_SLAVE_ADDRESS, bHour, 1);

  // return valid bits
  //hour is in 12 hour format
  if((bHourBuffer & 0x40) == 0x40)
  {
      return (bHourBuffer & 0x1F);
  }
  //hour is in 24 hour format
  else
  {
      return (bHourBuffer & 0x3F);
  }
}

/* ------------------------------------------------------------------- */
/** uint8_t getAmPm(uint8_t src)
**
**  Parameters:
**    src - RTCC_RTCC - real-time clock
**          RTCC_PWRD - Power-down time-stamp
**          RTCC_PWRU - Power-up time-stamp
**
**  Return Value:
**    uint8_t - 0 - RTCC_AM
**              1 - RTCC_PM
**
**  Description:
**    This function returns AM/PM for the source. This function should
**    only be used when the hour is in 12 hour format.
*/
uint8_t getAmPm(uint8_t src)
{
  uint8_t bHourBuffer = 0;
  uint8_t bHour;
  //Set address of the hour register depending on the source
  switch(src)
  {
    case RTCC_RTCC:
      bHour = 0x02;
      break;
    case RTCC_PWRD:
      bHour = 0x19;
      break;
    case RTCC_PWRU:
      bHour = 0x1D;
      break;
    default:
      return 0xFF; //ERROR Invalid Source
  }

  i2c_master_read_bytes(&bHourBuffer, I2C_SLAVE_ADDRESS, bHour, 1);

  return (bHourBuffer & 0x20);
}

/* ------------------------------------------------------------------- */
/** uint8_t getDay(uint8_t src)
**
**  Parameters:
**    src - RTCC_RTCC - real-time clock
**          RTCC_PWRD - Power-down time-stamp
**          RTCC_PWRU - Power-up time-stamp
**
**  Return Value:
**    uint8_t - day in hexadecimal
**
**  Description:
**    This function returns the day of the source
*/
uint8_t getDay(uint8_t src)
{
  uint8_t bDayBuffer = 0;
  unsigned bDay;
  //Set address of the day register depending on the source
  switch(src)
  {
    case RTCC_RTCC:
      bDay = 0x03;
      break;
    case RTCC_PWRD:
      bDay = 0x1B;
      break;
    case RTCC_PWRU:
      bDay = 0x1F;
      break;
    default:
      return 0xFF; //ERROR Invalid Source
  }

  i2c_master_read_bytes(&bDayBuffer, I2C_SLAVE_ADDRESS, bDay, 1);

    // return valid bits
  if(src == RTCC_PWRD || src == RTCC_PWRU)
  {
    return ((bDayBuffer & 0xE0)>> 5);
  }
  else
  {
     return (bDayBuffer & 0x07);
  }
}

/* ------------------------------------------------------------------- */
/** uint8_t getDate(uint8_t src)
**
**  Parameters:
**    src - RTCC_RTCC - real-time clock
**          RTCC_PWRD - Power-down time-stamp
**          RTCC_PWRU - Power-up time-stamp
**
**  Return Value:
**    uint8_t - date in hexadecimal
**
**  Description:
**    This function returns the date of the source
*/
uint8_t getDate(uint8_t src)
{
  uint8_t bDateBuffer = 0;
  unsigned bDate;

  //Set address of the date register depending on the source
  switch(src)
  {
    case RTCC_RTCC:
      bDate = 0x04;
      break;
    case RTCC_PWRD:
      bDate = 0x1A;
      break;
    case RTCC_PWRU:
      bDate = 0x1E;
      break;
    default:
      return 0xFF; //ERROR Invalid Source
  }

  i2c_master_read_bytes(&bDateBuffer, I2C_SLAVE_ADDRESS, bDate, 1);

  // return valid bits
  return (bDateBuffer & 0x3F);
}
/* ------------------------------------------------------------------- */
/** uint8_t getMonth(uint8_t src)
**
**  Parameters:
**    src - RTCC_RTCC - real-time clock
**          RTCC_PWRD - Power-down time-stamp
**          RTCC_PWRU - Power-up time-stamp
**
**  Return Value:
**    uint8_t - month in hexadecimal
**
**  Description:
**    This function returns the month of the source
*/
uint8_t getMonth(uint8_t src)
{
  uint8_t bMonthBuffer = 0;
  unsigned bMonth;

  //Set address of the month register depending on the source
  switch(src)
  {
    case RTCC_RTCC:
      bMonth = 0x05;
      break;
    case RTCC_PWRD:
      bMonth = 0x1B;
      break;
    case RTCC_PWRU:
      bMonth = 0x1F;
      break;
    default:
      return 0xFF; //ERROR Invalid Source
  }

  i2c_master_read_bytes(&bMonthBuffer, I2C_SLAVE_ADDRESS, bMonth, 1);

  // return valid bits
  return (bMonthBuffer & 0x1F);
}

/* ------------------------------------------------------------------- */
/** uint8_t getYear()
**
**  Parameters:
**    None
**
**  Return Value:
**    uint8_t - year in hexadecimal
**
**  Description:
**    This function returns the year of the RTCC_RTCC.
** The year parameter is only available for the RTCC_RTCC.
*/
uint8_t getYear()
{
  uint8_t bYearBuffer = 0;

  i2c_master_read_bytes(&bYearBuffer, I2C_SLAVE_ADDRESS, 0x6, 1);

  // return valid bits
  return (bYearBuffer & 0xFF);
}

//set function
/* ------------------------------------------------------------------- */
/** void setSec(uint8_t value)
**
**  Parameters:
**    value   - desire value for the second in HEX 0x00-0x59
**
**  Return Value:
**    None
**
**  Description:
**    This function sets the second of the dest with the value.
*/
void setSec(uint8_t value)
{
    uint8_t rgbSec = value;
    uint8_t temp;

    assert(value < 0x60);

    i2c_master_read_bytes(&temp, I2C_SLAVE_ADDRESS, 0, 1);

    if((temp & 0x80) == 0x80)
    {   //preserve configuration bits
        rgbSec = (rgbSec | 0x80);
    }

    //set second
    i2c_master_write_bytes(&rgbSec, I2C_SLAVE_ADDRESS, 0, 1);
}

/* ------------------------------------------------------------------- */
/** void setMin(uint8_t value)
**
**  Parameters:
**      value   - desire value for the minute in HEX 0x00-0x59
**
**  Return Value:
**    None
**
**  Description:
**    This function sets the minute of the dest with the value.
*/
void setMin(uint8_t value)
{
  uint8_t rgbMin = value;

  assert(value < 0x60);

  //set second
  i2c_master_write_bytes(&rgbMin, I2C_SLAVE_ADDRESS, 0x01, 1);
}

/* ------------------------------------------------------------------- */
/** void setHour(uint8_t value, uint8_t ampm)
**
**  Parameters:
**      value   - desire value for the hour in 12 hour format.
**                the value should be represented in HEX and should be
**                between 0x01-0x12
**      ampm  -  RTCC_AM
**               RTCC_PM
**
**  Return Value:
**    None
**
**  Description:
**      This function sets the hour of the dest with the value in
**  12 hour format.
*/
void setHour(uint8_t value, uint8_t ampm)
{
  uint8_t rgbHour;

  //validate hour 0x01-0x12
  assert((value < 0x13) && (value > 0));

  //set the format bit to 12 hr format
  //set ampm bit if PM
  if(ampm == RTCC_PM)
  {
      rgbHour = (value | 0x60);
  }
  else
  {
      rgbHour = (value | 0x40);
  }

  //set hour
  i2c_master_write_bytes(&rgbHour, I2C_SLAVE_ADDRESS, 0x2, 1);
}

/* ------------------------------------------------------------------- */
/** void setHour(uint8_t value)
**
**  Parameters:
**      value   - desire value for the hour in 24 hour format.
**                the value should be represented in HEX and should be
**                between 0x00-0x23
**
**  Return Value:
**    None
**
**  Description:
**  This function sets the hour of the destination with the value
**  in 24 hour format.
*/
void setHour(uint8_t value)
{
  uint8_t rgbHour = value;

  assert(value < 0x24);

  //set hour
  i2c_master_write_bytes(&rgbHour, I2C_SLAVE_ADDRESS, 0x2, 1);
}

/* ------------------------------------------------------------------- */
/** void setDay(uint8_t value)
**
**  Parameters:
**      value   - desire value for the day in HEX 0x01-0x07
**
**  Return Value:
**    None
**
**  Description:
**    This function sets the day of the dest with the value.
*/
void setDay(uint8_t value)
{
    uint8_t rgbDay;
    uint8_t temp;

    assert((value < 0x08) && (value > 0));

    i2c_master_read_bytes(&temp, I2C_SLAVE_ADDRESS, 0x3, 1);

    //reserve the control bits
    rgbDay = ((value & 0x07) | (temp & 0xF8));

    //set day
    i2c_master_write_bytes(&rgbDay, I2C_SLAVE_ADDRESS, 0x3, 1);
}

/* ------------------------------------------------------------------- */
/** void setDate(uint8_t value)
**
**  Parameters:
**      value   - desire value for the date in HEX 0x01-0x31
**
**  Return Value:
**    None
**
**  Description:
**    This function sets the date of the dest with the value.
*/
void setDate(uint8_t dest, uint8_t value)
{
  uint8_t rgbDate = value;

  assert((value < 0x32) && (value > 0));

  //set date
  i2c_master_write_bytes(&rgbDate  , I2C_SLAVE_ADDRESS, 0x4, 1);
}

/* ------------------------------------------------------------------- */
/** void setMonth(uint8_t value)
**
**  Parameters:
**      value   - desire value for the month in HEX 0x01-0x12
**
**  Return Value:
**    None
**
**  Description:
**    This function sets the month of the dest with the value.
*/
void setMonth(uint8_t dest, uint8_t value)
{
  uint8_t rgbMonth = value;

  assert((value < 0x13) && (value > 0));

  //set month
  i2c_master_write_bytes(&rgbMonth , I2C_SLAVE_ADDRESS, 0x5, 1);
}

/* ------------------------------------------------------------------- */
/** void setYear(uint8_t value)
**
**  Parameters:
**      value   - desire value for the month in HEX 0x00-0x99
**
**  Return Value:
**    None
**
**  Description:
**    This function sets the year of the RTCC_RTCC with the value.
**  The year parameter is only available for the RTCC_RTCC.
*/
void setYear(uint8_t value)
{
  uint8_t rgbYear = value;

  assert(value < 0xA0);

  //set year
  i2c_master_write_bytes(&rgbYear , I2C_SLAVE_ADDRESS, 0x6, 1);
}

