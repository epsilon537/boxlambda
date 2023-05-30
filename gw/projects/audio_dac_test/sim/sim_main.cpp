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

const char	DEFAULT_DAC_OUT_FILENAME[] = "dac_out.py";
const char	DEFAULT_PCM_OUT_FILENAME[] = "pcm_out.py";
FILE *dacOutFile = 0;
FILE *pcmOutFile = 0;
int dacOutputCounter = 0; 

bool tracing_enable = false;

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

// Legacy function required only so linking works on Cygwin and MSVC++
double sc_time_stamp() { return 0; }

//Clean-up logic.
static void cleanup() {
  //Close output files
  fprintf(dacOutFile, "]\n");
  fclose(dacOutFile);
  fprintf(pcmOutFile, "]\n");
  fclose(pcmOutFile);
  
  //Close trace file.
  if (tracing_enable)
    tfp->close();

  // Final model cleanup
  top->final();
}

//Advance simulation by one clock cycle
static void tick(void) {
  //High phase
  top->ext_clk = 1;
  contextp->timeInc(1);
  top->eval();
  if (tracing_enable)
    tfp->dump(contextp->time());
  
  //Low phase
  top->ext_clk = 0;
  contextp->timeInc(1);
  top->eval();
  if (tracing_enable)
    tfp->dump(contextp->time());

  //Capture output signals every 4 clocks, i.e. 12.5MHz
  ++dacOutputCounter;
  if (dacOutputCounter%4 == 0) {
    dacOutputCounter = 0;
    fprintf(dacOutFile, "  %d,\n", top->audio_out);
    fprintf(pcmOutFile, "  %d,\n", short(top->pcm_out));
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

    bool interactive_mode = false;
    const char *dac_out_filename = DEFAULT_DAC_OUT_FILENAME;
    const char *pcm_out_filename = DEFAULT_PCM_OUT_FILENAME;
    
    // Command line processing
    for(;;) {
      switch(getopt(argc, argv, "aith")) {
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
    
    printf("DAC Output File: %s\n", dac_out_filename);
    printf("PCM Output File: %s\n", pcm_out_filename);
    
    dacOutFile = fopen(dac_out_filename, "w");
    if (dacOutFile == NULL) {
      printf("Unable to open DAC output file\n");
      return -1;
    }
    fprintf(dacOutFile, "dacdata = [\n");

    pcmOutFile = fopen(pcm_out_filename, "w");
    if (pcmOutFile == NULL) {
      printf("Unable to open PCM output file\n");
      return -1;
    }
    fprintf(pcmOutFile, "pcmdata = [\n");

    // Assert reset for a couple of clock cycles.
    top->ext_clk = 0;
    top->ext_rst_n = !1;
    tick();
    tick();
    tick();
    tick();
    top->ext_rst_n = !0;
    top->sw = 4;
    tick();
    tick();
    tick();
    tick();
  
    // When not in interactive mode, simulate for 50000000 timeprecision periods
    while (interactive_mode || (contextp->time() < 50000000)) {
        // Evaluate model
        tick();        
    }
    
    cleanup();

    // Checks for automated testing.
    int res = 0;

    return res;
}
