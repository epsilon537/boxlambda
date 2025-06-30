#pragma once

#include <stdint.h>
#include <stddef.h>

#define SPI_MODE0 0x00
#define SPI_MODE1 0x04
#define SPI_MODE2 0x08
#define SPI_MODE3 0x0C
#define MOSI 0
#define MISO 0
#define SCK 0
#define PNUM_NOT_DEFINED 0

typedef enum {
  LSBFIRST = 0,
  MSBFIRST = 1,
} BitOrder;

class SPISettings {
public:
  constexpr SPISettings(uint32_t clock, BitOrder bitOrder, uint8_t dataMode)
  { }
  constexpr SPISettings()
  { }
};

class SPIClass {
public:
   inline SPIClass(uint32_t mosi = MOSI, uint32_t miso = MISO, uint32_t sclk = SCK, uint32_t ssel = PNUM_NOT_DEFINED) {}

   inline void begin(void) {}

  /* This function should be used to configure the SPI instance in case you
   * don't use default parameters.
   */
   inline void beginTransaction(SPISettings settings) {}
   inline void endTransaction(void) {}

  /* Transfer functions: must be called after initialization of the SPI
   * instance with begin() or beginTransaction().
   */
   inline uint8_t transfer(uint8_t data) { return 0; }
};

extern SPIClass SPI;

