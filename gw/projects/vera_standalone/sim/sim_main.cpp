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

//To get access to the verilated model internals.
#include "Vmodel___024root.h"

#include "vera.h"

// Legacy function required only so linking works on Cygwin and MSVC++
double sc_time_stamp() { return 0; }

//A very crude external bus write implementation.
void ext_bus_wr(VerilatedFstC* tfp,
                const std::unique_ptr<VerilatedContext>& contextp, 
                const std::unique_ptr<Vmodel>& top, unsigned addr, unsigned data) {
  top->extbus_a = addr;      /* Address */
  top->extbus_d = data;      /* Data (bi-directional) */

  top->extbus_cs_n = 0;   /* Chip select */
  top->extbus_wr_n = 0;   /* Write strobe */
  top->clk25 = 1;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());
  top->clk25 = 0;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());

  top->clk25 = 1;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());
  top->clk25 = 0;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());

  top->clk25 = 1;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());
  top->clk25 = 0;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());

  top->extbus_cs_n = !0;   /* Chip select */
  top->extbus_wr_n = !0;   /* Write strobe */
  top->clk25 = 1;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());

  top->clk25 = 0;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());

  top->clk25 = 1;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());
  top->clk25 = 0;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());

  top->clk25 = 1;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());
  top->clk25 = 0;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());

  top->clk25 = 1;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());
  top->clk25 = 0;
  contextp->timeInc(1);
  top->eval();
  if (tfp)
    tfp->dump(contextp->time());
}

int main(int argc, char** argv, char** env) {

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
      switch(getopt(argc, argv, "th")) {
      case 't':
        printf("Tracing enabled\n");
        tracing_enable = true;
        continue;

      case '?':
      case 'h':
      default :
        printf("\nVmodel Usage:\n");
        printf("-h: print this help\n");
        printf("-t: enable tracing.\n");
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

    // Construct the Verilated model, from Vmodel.h generated from Verilating this project.
    // Using unique_ptr is similar to "Vmodel* top = new Vmodel" then deleting at end.
    const std::unique_ptr<Vmodel> top{new Vmodel{contextp.get()}};

    //Trace file
    if (tracing_enable) {
      top->trace(tfp, 99); //Trace 99 levels deep.
      tfp->open("simx.fst");
    }
    
    // Set Vtop's input signals
    
    // External bus interface
    top->extbus_cs_n = !0;   /* Chip select */
    top->extbus_rd_n = !0;   /* Read strobe */
    top->extbus_wr_n = !0;   /* Write strobe */
    top->extbus_a = 0;      /* Address */
    top->extbus_d = 0;      /* Data (bi-directional) */

    top->clk25 = 0;
    contextp->timeInc(1);  // 1 timeprecision period passes...
    top->eval();

    //Let the synchronizer do its work. Spin until we come out of reset.
    while(contextp->time() < 1000) {
      top->clk25 = 0;
      contextp->timeInc(1);  // 1 timeprecision period passes...
      top->eval();
      top->clk25 = 1;
      contextp->timeInc(1);  // 1 timeprecision period passes...
      top->eval();
    }

    top->clk25 = 0;
    contextp->timeInc(1);  // 1 timeprecision period passes...
    top->eval();

    if (tracing_enable)
      tfp->dump(contextp->time());

    ext_bus_wr(tracing_enable? tfp : 0, contextp, top, VERA_DC_VIDEO, 0x71); //sprite ebable, Layer 1 enable, Layer 0 enable, VGA output mode.
    ext_bus_wr(tracing_enable? tfp : 0, contextp, top, VERA_L0_CONFIG, 0x23); //tile mode, 8bpp.
    ext_bus_wr(tracing_enable? tfp : 0, contextp, top, VERA_L0_TILEBASE, 0x0); //tile height/width 8.
    ext_bus_wr(tracing_enable? tfp : 0, contextp, top, VERA_L1_CONFIG, 0x23); //tile mode, 8bpp.
    ext_bus_wr(tracing_enable? tfp : 0, contextp, top, VERA_L1_TILEBASE, 0x0); //tile height/width 8.

    // When not in interactive mode, simulate for 1000000 timeprecision periods
    while (interactive_mode || (contextp->time() < 1000000)) {
        
        // Evaluate model
        top->clk25 = 1;
        contextp->timeInc(1);
        top->eval();
	
        if (tracing_enable)
          tfp->dump(contextp->time());
	
        top->clk25 = 0;
        contextp->timeInc(1);
        top->eval();
	
        if (tracing_enable)
          tfp->dump(contextp->time());

        //Positional printing using ncurses.
	      mvprintw(0, 0, "[%lld]", contextp->time());
	      refresh();
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

    return 0;
}
