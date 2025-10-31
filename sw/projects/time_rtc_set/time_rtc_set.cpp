/*
 * BoxLambda version of TimeRTCSet example program provided by TimeLib.
 *
 * Example code illustrating Time library with Real Time Clock.
 *
 * RTC clock is set in response to serial port time message
 * On Linux, you can use "date +T%s > /dev/ttyACM0" (UTC time zone)
 */

#include "TimeLib.h"
#include "i2c.h"
#include "MCP79412RTC.h" // a basic MCP79412 library that returns time as a time_t
#include "uart.h"
#include "delay.h"
#include <stdio.h>

extern "C" {
  //_init is executed by picolibc startup code before main().
  void _init(void) {
  }

  //_exit is executed by the picolibc exit function.
  //An implementation has to be provided to be able to user assert().
  void	_exit (int status) {
    while (1);
  }
}

void loop();
unsigned long processSyncMessage();
void digitalClockDisplay();

int main(void) {
  printf("Time RTC set example program.\n");

  setSyncProvider(RTC.get);   // the function to get the time from the RTC
  if (timeStatus() != timeSet)
     printf("Unable to sync with the RTC");
  else
     printf("RTC has set the system time");

  loop();

  return 0;
}

void loop()
{
  if (uart_rx_ready()) {
    time_t t = processSyncMessage();
    if (t != 0) {
      RTC.set(t);   // set the RTC and the system time to the received value
      setTime(t);
    }
  }
  digitalClockDisplay();
  delay(1000);
}

void digitalClockDisplay(){
  // digital clock display of the time
  printf("%02d:02d:02d %d %d %d\n",hour(), minute(), second(), day(), month(), year());
}

/*  code to process time sync messages from the serial port   */
#define TIME_HEADER  'T'   // Header tag for serial time sync message

unsigned long processSyncMessage() {
  unsigned long pctime = 0L;
  const unsigned long DEFAULT_TIME = 1357041600; // Jan 1 2013

  while (uart_rx() != TIME_HEADER);

  scanf("%lu", &pctime);
  if( pctime < DEFAULT_TIME) { // check the value is a valid time (greater than Jan 1 2013)
    pctime = 0L; // return 0 to indicate that the time is not valid
  }

  return pctime;
}

