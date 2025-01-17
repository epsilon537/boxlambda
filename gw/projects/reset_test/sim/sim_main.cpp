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

bool tracing_enable = false;

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
static void tick(unsigned gp_in, unsigned rst_ni) {
  // Tick twice: Input clock is 100MHz, BoxLambda's system clock runs at 50MHz.
  //->Advance two input clock cycles at a time.
  for (int ii = 0; ii < 2; ii++) {
    // High phase
    top->clk_i = 1;
    contextp->timeInc(1);
    top->eval();
    if (tracing_enable)
      tfp->dump(contextp->time());

    // Low phase
    top->clk_i = 0;
    contextp->timeInc(1);
    top->eval();
    if (tracing_enable)
      tfp->dump(contextp->time());
  }

  //The test sequence in main() drives the GPIO inputs via the gp_in variable
  top->gp_in = gp_in;

  //The test sequence in main() drives the external reset input via the rst_ni variable
  top->rst_ni = rst_ni;

  // Feed our model's uart_tx signal and baud rate to the UART co-simulator.
  // and feed the UART co-simulator output to our model
  top->uart_rx = (*uart)(
      top->uart_tx,
      top->rootp->sim_main__DOT__dut__DOT__boxlambda_soc_inst__DOT__wbuart_inst__DOT__uart_setup);

  // Detect and print changes to UART
  if (uart->get_rx_string().back() == '\n') {
    printf("DUT: %s", uart->get_rx_string().c_str());

    // Update change detectors
    uartRxStringPrev = uart->get_rx_string();

    uart->clear_rx_string();
  }
}

// This function implements a test sequence, triggering events
// such as a GPIO button press or asserting external reset based on the
// messages from the DUT it detects on the serial port.
int reset_test() {
  int done = 0;
  unsigned gp_in = 0;
  unsigned rst_ni = 1;

  while (!done && (contextp->time() < 15000000)) {
    // Evaluate model
    tick(gp_in, rst_ni);

    std::string uartCheckString("Reset Reason: Power-On Reset.");
    if (uartRxStringPrev.find(uartCheckString) != std::string::npos) {
      printf("SIM: String matched. Moving on...\n");
      done = 1;
    }
  }

  if (!done) {
    printf("SIM: Reset Reason Power-On Reset not detected. Test failed (0).\n");
    return -1;
  }

  done = 0;

  while (!done && (contextp->time() < 15000000)) {
    // Evaluate model
    tick(gp_in, rst_ni);

    std::string uartCheckString("Push btn[0] to SW trigger DM+NDM Reset.");

    if (uartRxStringPrev.find(uartCheckString) != std::string::npos) {
      printf("SIM: String matched. Moving on...\n");
      done = 1;
    }
  }

  if (!done) {
    printf("SIM: 'Push btn[0] not detected.' Test failed (1).\n");
    return -1;
  }

  done = 0;

  int button0pushed = 0;

  while (!done && (contextp->time() < 25000000)) {
    // Evaluate model
    tick(gp_in, rst_ni);

    //'Push' button 0.
    if (!button0pushed) {
        printf("SIM: Pushing button 0...\n");
        gp_in = 0x100;     //Push button 0.
        button0pushed = 1; //Indicate that button 0 has been pushed.
    }

    if (button0pushed &&
        top->rootp->sim_main__DOT__dut__DOT__boxlambda_soc_inst__DOT__reset_ctrl_inst__DOT__ndm_reset_o_reg &&
        top->rootp->sim_main__DOT__dut__DOT__boxlambda_soc_inst__DOT__reset_ctrl_inst__DOT__dm_reset_o_reg) {
      printf("SIM: NDM and DM reset trigger detected.\n");
      printf("SIM: Releasing button 0...\n");

      /*Release button 0*/
      gp_in = 0x0;

      done = 1;
    }
  }

  if (!done) {
    printf("SIM: NDM and DM reset failed to assert. Test failed (2).\n");
    return -1;
  }

  done = 0;

  while (!done && (contextp->time() < 35000000)) {
    // Evaluate model
    tick(gp_in, rst_ni);

    std::string uartCheckString("Reset Reason: SW triggered Non-Debug Module Reset.");
    if (uartRxStringPrev.find(uartCheckString) != std::string::npos) {
      printf("SIM: String matched. Moving on...\n");
      done = 1;
    }
  }

  if (!done) {
    printf("SIM: Reset reason Non-Debug Module reset not detected. Test failed (3).\n");
    return -1;
  }

  done = 0;

  while (!done && (contextp->time() < 35000000)) {
    // Evaluate model
    tick(gp_in, rst_ni);

    std::string uartCheckString("Reset Reason: SW triggered Debug Module Reset.");
    if (uartRxStringPrev.find(uartCheckString) != std::string::npos) {
      printf("SIM: String matched. Moving on...\n");
      done = 1;
    }
  }

  if (!done) {
    printf("SIM: Reset reason Debug Module Reset not detected. Test failed (4).\n");
    return -1;
  }

  //Now trigger an external reset
  printf("SIM: Asserting external reset...\n");

  rst_ni = 0;
  for (int ii=0; ii<1000000; ii++) {
    tick(gp_in, rst_ni);
  }
  rst_ni = 1;

  printf("SIM: Releasing external reset...\n");

  done = 0;

  while (!done && (contextp->time() < 45000000)) {
    // Evaluate model
    tick(gp_in, rst_ni);

    std::string uartCheckString("Reset Reason: External Reset.");
    if (uartRxStringPrev.find(uartCheckString) != std::string::npos) {
      printf("SIM: String matched. Moving on...\n");
      done = 1;
    }
  }

  if (!done) {
    printf("SIM: Reset reason External reset not detected. Test failed (5).\n");
    return -1;
  }

  return 0;
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

  // Command line processing
  for (;;) {
    switch (getopt(argc, argv, "ath")) {
    case 'a':
      attach_debugger = true;
      continue;
    case 't':
      printf("SIM: Tracing enabled\n");
      tracing_enable = true;
      continue;
    case '?':
    case 'h':
    default:
      printf("SIM: \nVmodel Usage:\n");
      printf("SIM: -h: print this help\n");
      printf("SIM: -a: attach debugger.\n");
      printf("SIM: -t: enable tracing.\n");
      return 0;
      break;

    case -1:
      break;
    }

    break;
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

  // Run the test sequence
  int res = reset_test();

  if (res == 0) {
    printf("SIM: Test passed.\n");
  }

  cleanup();

  return res;
}

