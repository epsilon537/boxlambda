#include <fcntl.h>
#include <getopt.h>

// For std::unique_ptr
#include <memory>

#include <stdio.h>
#include <string>

// Include common routines
#include <verilated.h>

// Include model header, generated from Verilating "top.v"
#include "Vmodel.h"

#include "verilated_fst_c.h"

// To get access to the verilated model internals.
#include "Vmodel___024root.h"

// From wbuart32
#include "uartsim.h"

// From riscv-dbg
#include "sim_jtag.h"

// From qspiflash
#include "flashsim.h"

const char *DEV_RANDOM = "/dev/urandom";
const unsigned FLASH_SW_IMG_OFFSET = 0x400000;

bool tracing_enable = false;

// SPI Flash co-simulation object from qspiflash repo.
std::unique_ptr<FLASHSIM> flash{new FLASHSIM(24/*lglen*/, false/*debug*/, 0/*rddelay*/, 0/*ndummy*/)};

// Uart co-simulation from wbuart32.
std::unique_ptr<UARTSIM> uart{new UARTSIM(0)};

// Used for tracing.
VerilatedFstC *tfp = new VerilatedFstC;

// Construct a VerilatedContext to hold simulation time, etc.
// Multiple modules (made later below with Vtop) may share the same
// context to share time, or modules may have different contexts if
// they should be independent from each other.
std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};

// Construct the Verilated model, from Vmodel.h generated from Verilating this
// project. Using unique_ptr is similar to "Vmodel* top = new Vmodel" then
// deleting at end.
std::unique_ptr<Vmodel> top{new Vmodel{contextp.get()}};

// Initialize UART rx and tx change detector
std::string uartRxStringPrev;

// Legacy function required only so linking works on Cygwin and MSVC++
double sc_time_stamp() { return 0; }

// Clean-up logic.
static void cleanup() {
  // Close trace file.
  if (tracing_enable)
    tfp->close();

  // Final model cleanup
  top->final();
}

// Advance simulation by one clock cycle
static void tick(void) {
  int spiflash_dat;

  // Tick twice: Input clock is 100MHz, BoxLambda's system clock runs at 50MHz.
  //->Advance two input clock cycles at a time.
  for (int ii = 0; ii < 2; ii++) {
    // High phase
    top->clk_i = 1;
    contextp->timeInc(1);
    top->eval();
    if (tracing_enable)
      tfp->dump(contextp->time());

    // Feed our model's spi flash signals to the flash co-simulator.
    // Convert from qspi interface expected by flashsim to single spi mosi/miso.
    spiflash_dat = top->spiflash_mosi;
    spiflash_dat = (*flash)(top->spiflash_cs_n, top->spiflash_sck, spiflash_dat);
    top->spiflash_miso = (spiflash_dat>>1)&1;

    // Low phase
    top->clk_i = 0;
    contextp->timeInc(1);
    top->eval();
    if (tracing_enable)
      tfp->dump(contextp->time());

    // Feed our model's spi flash signals to the flash co-simulator.
    // Convert from qspi interface expected by flashsim to single spi mosi/miso.
    spiflash_dat = top->spiflash_mosi;
    spiflash_dat = (*flash)(top->spiflash_cs_n, top->spiflash_sck, spiflash_dat);
    top->spiflash_miso = (spiflash_dat>>1)&1;
  }

  // Feed our model's uart_tx signal and baud rate to the UART co-simulator.
  // and feed the UART co-simulator output to our model
  top->uart_rx = (*uart)(
      top->uart_tx,
      top->rootp
          ->sim_main__DOT__dut__DOT__boxlambda_soc_inst__DOT__wbuart_inst__DOT__uart_setup);

  // Detect and print changes to UART
  if (uart->get_rx_string().back() == '\n') {
    printf("DUT: %s", uart->get_rx_string().c_str());

    // Update change detectors
    uartRxStringPrev = uart->get_rx_string();

    uart->clear_rx_string();
  }
}

int main(int argc, char **argv, char **env) {
  // Prevent unused variable warnings
  if (false && argc && argv && env) {
  }

  // Set debug level, 0 is off, 9 is highest presently used
  // May be overridden by commandArgs argument parsing
  contextp->debug(0);

  // Randomization reset policy
  // May be overridden by commandArgs argument parsing
  contextp->randReset(2);

  // Verilator must compute traced signals
  contextp->traceEverOn(true);

  // Pass arguments so Verilated code can see them, e.g. $value$plusargs
  // This needs to be called before you create any model
  contextp->commandArgs(argc, argv);

  bool attach_debugger = false;
  bool interactive_mode = false;
  const char *flash_img_filename = 0;

  // Command line processing
  for (;;) {
    switch (getopt(argc, argv, "aithf:")) {
    case 'a':
      attach_debugger = true;
      continue;
    case 'f':
      flash_img_filename = optarg;
      continue;
    case 't':
      printf("SIM: Tracing enabled\n");
      tracing_enable = true;
      continue;
    case 'i':
      printf("SIM: Interactive mode enabled\n");
      interactive_mode = true;
      continue;
    case '?':
    case 'h':
    default:
      printf("SIM: \nVmodel Usage:\n");
      printf("SIM: -h: print this help\n");
      printf("SIM: -f <flash_sw.img>\n");
      printf("SIM: -a: attach debugger.\n");
      printf("SIM: -t: enable tracing.\n");
      printf("SIM: -i: enable interactive mode.\n");
      return 0;
      break;

    case -1:
      break;
    }

    break;
  }

  //  flash->debug(true);
  if (!flash_img_filename) {
    if (flash->load(0, DEV_RANDOM) < 0) {
      printf("SIM: Error loading dev_random.\n");
      return -1;
    }
  } else {
    printf("SIM: Flash SW Image File: %s\n", flash_img_filename);
    if (flash->load(FLASH_SW_IMG_OFFSET, flash_img_filename) < 0) {
      printf("SIM: Error loading dev_random.\n");
      return -1;
    }
  }

  // Trace file
  if (tracing_enable) {
    top->trace(tfp, 99); // Trace 99 levels deep.
    tfp->open("simx.fst");
  }

  jtag_set_bypass(!attach_debugger);

  top->clk_i = 0;
  top->uart_rx = 0;

  // Take the system out of reset.
  top->rst_ni = 1;

  // When not in interactive mode, simulate for 150000000 timeprecision periods
  while (interactive_mode || (contextp->time() < 150000000)) {
    // Evaluate model
    tick();
  }

  int res = 0;
  std::string uartCheckString("Test Successful.");

  if (uartRxStringPrev.find(uartCheckString) == std::string::npos) {
    printf("SIM: Test failed\n");
    printf("SIM: Expected: %s\n", uartCheckString.c_str());
    printf("SIM: Received: %s\n", uartRxStringPrev.c_str());

    res = 1;
  } else {
    printf("SIM: Test passed.\n");
  }

  cleanup();

  return res;
}

