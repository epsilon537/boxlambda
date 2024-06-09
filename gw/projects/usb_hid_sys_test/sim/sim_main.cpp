//#include <curses.h>
#include <getopt.h>
#include <fcntl.h>

// For std::unique_ptr
#include <memory>

#include <string>
#include <stdio.h>

// Include common routines
#include <verilated.h>

// Include model header, generated from Verilating "top.v"
#include "Vmodel.h"

#include "verilated_fst_c.h"

//To get access to the verilated model internals.
#include "Vmodel___024root.h"

// From wbuart32
#include "uartsim.h"

// From riscv-dbg
#include "sim_jtag.h"

bool tracing_enable = false;

int prev_ledg_0 = 0;
int prev_ledr_0 = 0;
int prev_ledg_1 = 0;
int prev_ledr_1 = 0;

int ledg_acc = 0;

//Uart co-simulation from wbuart32.
std::unique_ptr<UARTSIM> uart{new UARTSIM(0)};

// Used for tracing.
VerilatedFstC* tfp = new VerilatedFstC;

// Construct a VerilatedContext to hold simulation time, etc.
// Multiple modules (made later below with Vtop) may share the same
// context to share time, or modules may have different contexts if
// they should be independent from each other.
std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};

// Construct the Verilated model, from Vmodel.h generated from Verilating this project.
// Using unique_ptr is similar to "Vmodel* top = new Vmodel" then deleting at end.
std::unique_ptr<Vmodel> top{new Vmodel{contextp.get()}};

//Initialize UART rx buffer
std::string uartRxString;

// Legacy function required only so linking works on Cygwin and MSVC++
double sc_time_stamp() { return 0; }

//Clean-up logic.
static void cleanup() {
  //Close trace file.
  if (tracing_enable)
    tfp->close();

  // Final model cleanup
  top->final();
}

//One clock cycle of the 100MHz clock.
static void tick100(void) {
  top->clk_100 = 1;
  contextp->timeInc(1);
  top->eval();
  if (tracing_enable)
    tfp->dump(contextp->time());

  top->clk_100 = 0;
  contextp->timeInc(1);
  top->eval();
  if (tracing_enable)
    tfp->dump(contextp->time());
}

//One clock cycle of the 50MHz clock.
static void tick50(void) {
  //Two 100MHz clock cycles in one 50MHz clock cycle.
  top->clk_50 = 1;
  tick100();

  top->clk_50 = 0;
  tick100();

  //Set GPIO0 bit 4 (switch 0) to indicate to SW we want rotating keyboard LEDs
  top->gpio0 = 0x10;

  //Feed our model's uart_tx signal and baud rate to the UART co-simulator.
  //and feed the UART co-simulator output to our model
  top->uart_rx = (*uart)(top->uart_tx,
  top->rootp->sim_main__DOT__dut__DOT__boxlambda_soc_inst__DOT__wbuart_inst__DOT__uart_setup);

  //Detect and print changes to UART
  if (uart->get_rx_string().back() == '\n')  {
    printf("%s", uart->get_rx_string().c_str());

    //Accumulate the UART output in a uartRxString buffer, for analysis when the test has completed.
    uartRxString += uart->get_rx_string();

    uart->clear_rx_string();
  }

  //These LEDS come from the USB device cores. The ledg LEDS correspond to USB keyboard LEDs.
  if (top->ledg_1 != prev_ledg_1) {
    printf("ledg_1 = %d\n", top->ledg_1);
    ledg_acc |= top->ledg_1; //Keep track of which keyboard LEDs have been turned on.
    prev_ledg_1 = top->ledg_1;
  }

  if (top->ledr_0 != prev_ledr_0) {
    printf("ledr_0 = %d\n", top->ledr_0);

    prev_ledr_0 = top->ledr_0;
  }

  if (top->ledg_1 != prev_ledg_1) {
    printf("ledg_1 = %d\n", top->ledg_1);

    prev_ledg_1 = top->ledg_1;
  }

  if (top->ledr_1 != prev_ledr_1) {
    printf("ledr_1 = %d\n", top->ledr_1);

    prev_ledr_1 = top->ledr_1;
  }
}

static void tick6(void) {
  top->clk_6 = 1;

  //8 50MHz clock cycles in one 6.25MHz clock cycle.

  tick50();
  tick50();
  tick50();
  tick50();

  top->clk_6 = 0;

  tick50();
  tick50();
  tick50();
  tick50();
}

//We have a 6.25Mhz clock (for USB), a 50MHz clock, and a 100MHz clock.
//Advance simulation by one 6.25Mhz clock cycle.
static void tick(void) {
  tick6();
}

int main(int argc, char** argv, char** env) {
    // Prevent unused variable warnings
    if (false && argc && argv && env) {}

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

    // Command line processing
    for(;;) {
      switch(getopt(argc, argv, "aith")) {
      case 'a':
        attach_debugger = true;
        continue;
      case 't':
        printf("Tracing enabled\n");
        tracing_enable = true;
        continue;
      case 'i':
        printf("Interactive mode enabled\n");
        interactive_mode = true;
        continue;
      case '?':
      case 'h':
      default :
        printf("\nVmodel Usage:\n");
        printf("-h: print this help\n");
        printf("-a: attach debugger.\n");
        printf("-t: enable tracing.\n");
        printf("-i: enable interactive mode.\n");
        return 0;
        break;

      case -1:
        break;
      }

      break;
    }

    //Trace file
    if (tracing_enable) {
      top->trace(tfp, 99); //Trace 99 levels deep.
      tfp->open("simx.fst");
    }

    jtag_set_bypass(!attach_debugger);

    // Assert reset for a couple of clock cycles.
    top->clk_100 = 0;
    top->clk_50 = 0;
    top->clk_6 = 0;

    top->uart_rx = 0;

    top->rst_ni = 0;
    for (int ii=0;ii<32;ii++) {
      tick();
    }

    //Take the system out of reset.
    top->rst_ni = 1;

    // When not in interactive mode, simulate for 200000000 timeprecision periods
    while (interactive_mode || (contextp->time() < 200000000)) {
      // Evaluate model
      tick();
    }

    //Count keyboard reports in the UARTRxString buffer.
    int numKeyboardReports = 0;
    int pos=0;

    while (pos != std::string::npos) {
      pos = uartRxString.find("keyboard report", pos);
      if (pos != std::string::npos) {
        ++numKeyboardReports;
        ++pos;
      }
    }

    //Count mouse reports in the UARTRxString buffer.
    int numMouseReports = 0;
    pos = 0;

    while (pos != std::string::npos) {
      pos = uartRxString.find("mouse report", pos);
      if (pos != std::string::npos) {
        ++numMouseReports;
        ++pos;
      }
    }

    //Count 'Led Report IRQ not received' reports in the UARTRxString buffer.
    int numMissingIRQs = 0;
    pos = 0;

    while (pos != std::string::npos) {
      pos = uartRxString.find("Led Report IRQ not received", pos);
      if (pos != std::string::npos) {
        ++numMissingIRQs;
        ++pos;
      }
    }

    int res = 0;

    //We want at least 10 keyboard reports, 10 mouse reports, 0 missing IRQs and all leds turned on at least once;
    if ((numKeyboardReports >= 10) &&
        (numMouseReports >= 10) &&
        (numMissingIRQs == 0) &&
        (ledg_acc == 0x7)) {
      printf("Test passed.\n");
      res = 0;
    }
    else {
      printf("Test failed: numKeyboardReports: %d, numMouseReports: %d, LED acc.: 0x%x\n",
            numKeyboardReports, numMouseReports, ledg_acc);
      res = 1;
    }

    cleanup();
    return 0;
}
