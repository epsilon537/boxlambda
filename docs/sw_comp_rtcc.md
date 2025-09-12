---
hide:
  - toc
---

# Real-Time Clock and Calendar API

- **MCP79412RTC Repo**, BoxLambda fork, `boxlambda` branch:
    [https://github.com/epsilon537/MCP79412RTC](https://github.com/epsilon537/MCP79412RTC)

- **MCP79412RTC Submodule in the BoxLambda Directory Tree**:
    boxlambda/sub/MCP79412RTC/.

- **RTCC Software Component in the BoxLambda Directory Tree**:
  [boxlambda/sw/components/rtcc](https://github.com/epsilon537/boxlambda/tree/master/sw/components/rtcc)

The Real-Time Clock and Calendar (RTCC) software component is based on the [MCP79412RTC](https://github.com/epsilon537/MCP79412RTC) library. The library interfaces with Digilent's [PMOD RTCC](components_rtcc.md) via I2C.

The library allows you to set and get a time and date (among other things). The RTCC's coin cell battery backup ensures that the time and date will be maintained while the host (BoxLambda) is turned off.

Example usage:

```
  /*Time elements structure*/
  tmElements_t tmElements;

  RTC.begin();

  if (RTC.isRunning()) {
    printf("RTC oscillator is already running.\n");
  }
  else {
    printf("RTC oscillator is not running yet. Enabling...\n");

    RTC.vbaten(true);

    /*Some bogus initial values.*/
    tmElements.Second = 1;
    tmElements.Minute = 2;
    tmElements.Hour = 3;
    tmElements.Wday = 4;   // day of week, sunday is day 1
    tmElements.Day = 5;
    tmElements.Month = 6;
    tmElements.Year = 7;   // offset from 1970;

    RTC.write(tmElements);
  }

  uint8_t prevSecond = 0;

  for (;;) {
    RTC.read(tmElements);

    /*print a line when the seconds change.*/
    if (tmElements.Second != prevSecond) {
      prevSecond = tmElements.Second;

      printf("sec: 0x%x, min: 0x%x, hour: 0x%x.\n", (unsigned)tmElements.Second, (unsigned)tmElements.Minute, (unsigned)tmElements.Hour);
    }
  }
```
For additional example code, check the `rtcc_test` app:

- [boxlambda/sw/projects/rtcc_test/rtcc_test.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/rtcc_test/rtcc_test.cpp)
- [boxlambda/sw/projects/rtcc_test/rtcc_cli.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/rtcc_test/rtcc_cli.cpp)

