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

//We set GPIO1 bits 3:0 to 0xf to indicate to RISCV SW that this is a simulation.
#define GPIO1_SIM_INDICATOR 0xf 

bool tracing_enable = false;

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

//Initialize UART rx and tx change detector
std::string uartRxStringPrev;

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

//Advance simulation by one clock cycle
static void tick(void) {
  //Tick twice: Input clock is 100MHz, BoxLambda's system clock runs at 50MHz.
  //->Advance two input clock cycles at a time.
  for (int ii=0; ii<2;ii++) {
    //High phase
    top->clk_i = 1;
    contextp->timeInc(1);
    top->eval();
    if (tracing_enable)
      tfp->dump(contextp->time());
    
    //Low phase
    top->clk_i = 0;
    contextp->timeInc(1);
    top->eval();
    if (tracing_enable)
      tfp->dump(contextp->time());
  }
  
  top->gpio1 = GPIO1_SIM_INDICATOR; //Indicate to SW that this is a simulation.
  
  //Feed our model's uart_tx signal and baud rate to the UART co-simulator.
  //and feed the UART co-simulator output to our model
  top->uart_rx = (*uart)(top->uart_tx, 
  top->rootp->sim_main__DOT__dut__DOT__boxlambda_soc_inst__DOT__wb_uart__DOT__wbuart__DOT__uart_setup);

  //Detect and print changes to UART
  if (uart->get_rx_string().back() == '\n')  {
    printf("%s", uart->get_rx_string().c_str());

    //Update change detectors
    uartRxStringPrev = uart->get_rx_string();

    uart->clear_rx_string();
  }
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
    top->clk_i = 0;
    top->uart_rx = 0;

    //Take the system out of reset.
    top->rst_ni = 1;
    
    // When not in interactive mode, simulate for 20000000 timeprecision periods
    while (interactive_mode || (contextp->time() < 20000000)) {
      // Evaluate model
      tick();        
    }
    
    int res = 0;
    std::string uartCheckString("Test Successful.");

    if (uartRxStringPrev.find(uartCheckString) == std::string::npos) {
      printf("Test failed\n");
      printf("Expected: %s\n", uartCheckString.c_str());
      printf("Received: %s\n", uartRxStringPrev.c_str());

      res = 1;
    }
    else {
      printf("Test passed.\n");
    }

    cleanup();

    return 0;
}
