#include "rtcc_cli.h"

#define EMBEDDED_CLI_IMPL
#include "embedded_cli.h"
#include "embedded_cli_setup.h"

#include "MCP79412RTC.h"
#include "TimeLib.h"
#include <assert.h>
#include <stdio.h>

static struct uart *uartp = 0;

extern "C" {
  static void settime(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) < 3) {
        printf("Argument missing: settime <second> <minute> <hour>\n");
    }
    else {
      uint8_t second, minute, hour;

      char *token;

      token = embeddedCliGetTokenVariable(args, 1);
      sscanf(token, "%hhd", &hour);

      token = embeddedCliGetTokenVariable(args, 2);
      sscanf(token, "%hhd", &minute);

      token = embeddedCliGetTokenVariable(args, 3);
      sscanf(token, "%hhd", &second);

      tmElements_t tmElements;

      RTC.read(tmElements);
      tmElements.Second = second;
      tmElements.Minute = minute;
      tmElements.Hour = hour;
      RTC.write(tmElements);

      printf("Done.\n");
    }
  }

  static void gettime(EmbeddedCli *cli, char *args, void *context) {
      tmElements_t tmElements;
      RTC.read(tmElements);
      printf("The current time is (hh:mm:ss): %.2d:%.2d:%.2d.\n", tmElements.Hour, tmElements.Minute, tmElements.Second);
  }

  static void setdate(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) < 3) {
        printf("Argument(s) missing: setdate <day> <month> <year>\n");
    }
    else {
      uint8_t day, month, yearOffset;
      uint32_t year;

      char *token;

      token = embeddedCliGetTokenVariable(args, 1);
      sscanf(token, "%hhd", &day);

      token = embeddedCliGetTokenVariable(args, 2);
      sscanf(token, "%hhd", &month);

      token = embeddedCliGetTokenVariable(args, 3);
      sscanf(token, "%d", &year);

      if ((year < 1972) || (year >= 1972+256)) {
        printf("Year must be in range %d %d.\n", 1972, 1972+256);
      }

      yearOffset = (year-1972);

      tmElements_t tmElements;

      RTC.read(tmElements);
      tmElements.Day = day;
      tmElements.Month = month;
      tmElements.Year = yearOffset;
      RTC.write(tmElements);

      printf("Done.\n");
    }
  }

  static void getdate(EmbeddedCli *cli, char *args, void *context) {
      tmElements_t tmElements;
      RTC.read(tmElements);
      printf("The current date is (dd/mm/yyyy): %.2d/%.2d/%.4d.\n", tmElements.Day, tmElements.Month, tmElements.Year+1972);
  }
}

void add_rtcc_cli(EmbeddedCli* cli) {
  assert(cli);

  embeddedCliAddBinding(cli, {
        "settime",          // command name (spaces are not allowed)
        "settime <hour> <minute> <second>, e.g. 16 56 00",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        settime               // binding function
  });

  embeddedCliAddBinding(cli, {
        "gettime",          // command name (spaces are not allowed)
        "Get Current Time",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        gettime               // binding function
  });

  embeddedCliAddBinding(cli, {
        "setdate",          // command name (spaces are not allowed)
        "setdate <day> <month> <year> e.g. 23 7 2024",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        setdate               // binding function
  });

  embeddedCliAddBinding(cli, {
        "getdate",          // command name (spaces are not allowed)
        "Get Current Date",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        getdate               // binding function
  });
}

