// DESCRIPTION: This files is based the hello_world test build, modified
// to fit the hello_dbg test build. That means: no ncurses and
// no UART or GPIO checks. The purpose of of the build is to allow
// OpenOCD to connect to it.
//
// The hello_world test build in turn is based on Verilator example code, 
//======================================================================

#include <curses.h>
#include <getopt.h>
#include <fcntl.h>

// For std::unique_ptr
#include <memory>

#include <string>

// Include common routines
#include <verilated.h>

// Include model header, generated from Verilating "top.v"
#include "Vmodel.h"

#include "verilated_fst_c.h"

// From wbuart32
#include "uartsim.h"

// From riscv-dbg
#include "sim_jtag.h"

//To get access to the verilated model internals.
#include "Vmodel___024root.h"

//We set GPIO1 bits 3:0 to 0xf to indicate to RISCV SW that this is a simulation.
static const int GPIO1_SIM_INDICATOR = 0xf; 

static const char INPUT_TEST_CHAR = 's';

// Legacy function required only so linking works on Cygwin and MSVC++
double sc_time_stamp() { return 0; }

int main(int argc, char** argv, char** env) {
    //Uart co-simulation from wbuart32.
    std::unique_ptr<UARTSIM> uart{new UARTSIM(0)};

    // Prevent unused variable warnings
    if (false && argc && argv && env) {}

    // Construct a VerilatedContext to hold simulation time, etc.
    // Multiple modules (made later below with Vtop) may share the same
    // context to share time, or modules may have different contexts if
    // they should be independent from each other.

    // Using unique_ptr is similar to
    // "VerilatedContext* contextp = new VerilatedContext" then deleting at end.
    const std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};
    
    // Set debug level, 0 is off, 9 is highest presently used
    // May be overridden by commandArgs argument parsing
    contextp->debug(0);

    // Randomization reset policy
    // May be overridden by commandArgs argument parsing
    contextp->randReset(2);

    // Verilator must compute traced signals
    contextp->traceEverOn(true);

    VerilatedFstC* tfp = new VerilatedFstC;
    
    // Pass arguments so Verilated code can see them, e.g. $value$plusargs
    // This needs to be called before you create any model
    contextp->commandArgs(argc, argv);

    bool tracing_enable = false;
    bool attach_debugger = false;
    bool interactive_mode = false;
    
    // Command line processing
    for(;;) {
      switch(getopt(argc, argv, "ithd")) {
      case 'i':
        printf("Interactive mode\n");
        interactive_mode = true;
        continue;
	  
      case 't':
        printf("Tracing enabled\n");
        tracing_enable = true;
        continue;

      case 'd':
        printf("Attach debugger.\n");
        attach_debugger = true;
        continue;

      case '?':
      case 'h':
      default :
        printf("\nVmodel Usage:\n");
        printf("-h: print this help\n");
        printf("-i: interactive mode.\n");
        printf("-t: enable tracing.\n");
        printf("-d: attach debugger.\n");
        return 0;
        break;
	    
      case -1:
        break;
      }

      break;
    }

    //Curses setup
    initscr();
    cbreak();
    noecho();

    // Construct the Verilated model, from Vmodel.h generated from Verilating "ibex_soc.sv".
    // Using unique_ptr is similar to "Vmodel* top = new Vmodel" then deleting at end.
    // "ibex_soc" will be the hierarchical name of the module.
    const std::unique_ptr<Vmodel> top{new Vmodel{contextp.get(), "ddr_test_soc"}};

    //Trace file
    if (tracing_enable) {
      top->trace(tfp, 99); //Trace 99 levels deep.
      tfp->open("simx.fst");
    }
    
    // Set Vtop's input signals
    top->rst_ni = !0;
    top->clk_i = 0;
    top->uart_rx = 0;

    //Initialize GPIO change detectors
    unsigned char gpio0Prev = 0, gpio1Prev = 0;
    //Initialize UART rx and tx change detector
    std::string uartRxStringPrev;
    std::string uartTxStringPrev;
    
    //Accumulate GPIO0 value changes as a string into this variable
    std::string gpio0String;

    jtag_set_bypass(!attach_debugger);
    
    // When not in interactive mode, simulate for 10000000 timeprecision periods
    while (interactive_mode || (contextp->time() < 10000000)) {
        // Historical note, before Verilator 4.200 Verilated::gotFinish()
        // was used above in place of contextp->gotFinish().
        // Most of the contextp-> calls can use Verilated:: calls instead;
        // the Verilated:: versions simply assume there's a single context
        // being used (per thread).  It's faster and clearer to use the
        // newer contextp-> versions.

        contextp->timeInc(1);  // 1 timeprecision period passes...
        // Historical note, before Verilator 4.200 a sc_time_stamp()
        // function was required instead of using timeInc.  Once timeInc()
        // is called (with non-zero), the Verilated libraries assume the
        // new API, and sc_time_stamp() will no longer work.

        // Toggle control signals on an edge that doesn't correspond
        // to where the controls are sampled; in this example we do
        // this only on a negedge of clk, because we know
        // reset is not sampled there.
        if (!top->clk_i) {
          if (contextp->time() > 1 && contextp->time() < 10) {
            top->rst_ni = !1;  // Assert reset
          } else {
            top->rst_ni = !0;  // Deassert reset
          }
        }
	
        // Evaluate model
        top->clk_i = 1;
        top->eval();
	
        if (tracing_enable)
          tfp->dump(contextp->time());

        contextp->timeInc(1);
	
        top->gpio1 = GPIO1_SIM_INDICATOR; //Indicate to SW that this is a simulation.
        top->clk_i = 0;
        top->eval();
	
        if (tracing_enable)
          tfp->dump(contextp->time());

        //Feed our model's uart_tx signal and baud rate to the UART co-simulator.
        //and feed the UART co-simulator output to our model
        top->uart_rx = (*uart)(top->uart_tx, top->rootp->sim_main__DOT__dut__DOT__wb_uart__DOT__wbuart__DOT__uart_setup);

        //Detect and print changes to UART and GPIOs
        if ((uartRxStringPrev != uart->get_rx_string()) ||
            (uartTxStringPrev != uart->get_tx_string()) ||
            (gpio0Prev != top->gpio0) ||
            (gpio1Prev != top->gpio1)) {

          if (gpio0Prev != top->gpio0) {
            //Single digit int to hex conversion and accumulation into gpio0String.
            static const char* digits = "0123456789ABCDEF";
            gpio0String.push_back(digits[top->gpio0&0xf]);
          };

          //Positional printing using ncurses.
          mvprintw(0, 0, "[%lld]", contextp->time());
          mvprintw(1, 0, "UART Out:");
          mvprintw(2, 0, uart->get_rx_string().c_str());
          mvprintw(23, 0, "UART In:");
          mvprintw(24, 0, uart->get_tx_string().c_str());
          mvprintw(25, 0, "GPIO0: %x", top->gpio0);
          mvprintw(26, 0, "GPIO1: %x", top->gpio1);
          refresh();

          //Update change detectors
          uartRxStringPrev = uart->get_rx_string();
          uartTxStringPrev = uart->get_tx_string();

          gpio0Prev = top->gpio0;
          gpio1Prev = top->gpio1;
        }
    }

    //Close trace file.
    if (tracing_enable)
      tfp->close();
    
    // Final model cleanup
    top->final();

    // Coverage analysis (calling write only after the test is known to pass)
#if VM_COVERAGE
    contextp->coveragep()->write("logs/coverage.dat");
#endif
    // End curses.
    endwin();

    // Checks for automated testing.
    int res = 0;
    std::string uartCheckString("Memory Test successful.");

    if (uartRxStringPrev.find(uartCheckString) == std::string::npos) {
      printf("UART check failed\n");
      printf("Expected: %s\n", uartCheckString.c_str());
      printf("Received: %s\n", uartRxStringPrev.c_str());

      res = 1;
    }
    else {
      printf("UART check passed.\n");
    }

    // Return completion status
    // Don't use exit() or destructor won't get called
    return res;
}
