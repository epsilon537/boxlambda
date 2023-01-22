// DESCRIPTION: This files is based the hello_world test build, modified
// to fit the hello_dbg test build. That means: no ncurses and
// no UART or GPIO checks. The purpose of of the build is to allow
// OpenOCD to connect to it.
//
// The hello_world test build in turn is based on Verilator example code, 
//======================================================================

#include <getopt.h>

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
#define GPIO1_SIM_INDICATOR 0xf 

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
    
    // Command line processing
    for(;;) {
	switch(getopt(argc, argv, "thd")) {	  
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
	    printf("-t: enable tracing.\n");
	    printf("-d: attach debugger.\n");
	    return 0;
	    break;
	    
	  case -1:
	    break;
	}

	break;
    }
    
    // Construct the Verilated model, from Vmodel.h generated from Verilating the project.
    // Using unique_ptr is similar to "Vmodel* top = new Vmodel" then deleting at end.
    const std::unique_ptr<Vmodel> top{new Vmodel{contextp.get()}};

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
    //Initialize UART rx change detector
    std::string uartRxStringPrev;
    //Accumulate GPIO0 value changes as a string into this variable
    std::string gpio0String;

    //Bypass JTAG socket setup when attach debugger command line flag
    //is not set.
    jtag_set_bypass(!attach_debugger);
    
    while (1 /*contextp->time() < 10000000*/) {
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
	(*uart)(top->uart_tx, top->rootp->sim_main__DOT__dut__DOT__wb_uart__DOT__wbuart__DOT__uart_setup);
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
    // Return completion status
    // Don't use exit() or destructor won't get called
    return 0;
}
