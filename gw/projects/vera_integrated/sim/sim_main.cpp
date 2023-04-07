#include <curses.h>
#include <getopt.h>
#include <fcntl.h>

// For std::unique_ptr
#include <memory>

#include <string>

#include <SDL2/SDL.h>

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
static const int GPIO1_SIM_INDICATOR = 0xf; 

static const char INPUT_TEST_CHAR = 's';

//SDL objects:
#define SDL_WINDOW_WIDTH 800

SDL_Event sdl_event;
SDL_Renderer *sdl_renderer;
SDL_Window *sdl_window;
SDL_Texture *sdl_display;

int sdl_x=0/*750*/, sdl_y=0/*523*/;

bool vsync_prev = true; 
bool hsync_prev = true;
bool exit_req = false;

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

//Initialize GPIO change detectors
unsigned char gpio0Prev = 0, gpio1Prev = 0;
//Initialize UART rx and tx change detector
std::string uartRxStringPrev;
std::string uartTxStringPrev;

//Accumulate GPIO0 value changes as a string into this variable
std::string gpio0String;

unsigned framecount = 0;
FILE *frameFile = 0;

// Legacy function required only so linking works on Cygwin and MSVC++
double sc_time_stamp() { return 0; }

void cleanup() {
  // End curses.
  endwin();

  //SDL clean-up.
  SDL_DestroyRenderer(sdl_renderer);
  SDL_DestroyWindow(sdl_window);
  SDL_Quit();

  //Close trace file.
  if (tracing_enable)
    tfp->close();

  // Final model cleanup
  top->final();
}

//Advance simulation by one clock cycle
static void tick(void) {
  top->clk_i = 1;
  contextp->timeInc(1);
  top->eval();
  if (tracing_enable)
    tfp->dump(contextp->time());
  top->clk_i = 0;
  contextp->timeInc(1);
  top->eval();
  if (tracing_enable)
    tfp->dump(contextp->time());

  //Exit if user closes the SDL window.
  if (SDL_PollEvent(&sdl_event) && sdl_event.type == SDL_QUIT)
    exit_req = true;

  //Clear the screen during Vsync
  if (!top->vga_vsync && vsync_prev) {
    SDL_SetRenderDrawColor(sdl_renderer, 0, 0, 0, 0);
    SDL_RenderClear(sdl_renderer);
    sdl_y = 0;
    ++framecount;

    if (framecount == 2)
      frameFile = fopen("frame.bin", "wb");

    if (framecount == 3) {
      mvprintw(0, 0, "[%lld]", contextp->time());
      mvprintw(23, 0, "Closing frame file...\n\r");
      refresh();
      fclose(frameFile);
      frameFile = 0;
    }
  }

  //Render to SDL's back buffer at each Hsync.
  if (!top->vga_hsync && hsync_prev) {
    SDL_SetRenderTarget(sdl_renderer, NULL);
    SDL_RenderCopy(sdl_renderer, sdl_display, NULL, NULL);
    SDL_RenderPresent(sdl_renderer);
    SDL_SetRenderTarget(sdl_renderer, sdl_display);
    sdl_x = 0;
    sdl_y++;
  }

  static Uint8 pixel[4];

  pixel[0] = (Uint8)(top->vga_r<<4);
  pixel[1] = (Uint8)(top->vga_g<<4);
  pixel[2] = (Uint8)(top->vga_b<<4);
  pixel[3] = 255;
  
  //Render the VGA rgb output. Convert RGB4:4:4 to RGB8:8:8.
  SDL_SetRenderDrawColor(sdl_renderer, pixel[0], pixel[1], pixel[2], pixel[3]);
  SDL_RenderDrawPoint(sdl_renderer, sdl_x>>1, sdl_y);
  
  if (frameFile)
    fwrite(pixel, 1, 4, frameFile);

  ++sdl_x;

  vsync_prev = top->vga_vsync;
  hsync_prev = top->vga_hsync;

  //Feed our model's uart_tx signal and baud rate to the UART co-simulator.
  //and feed the UART co-simulator output to our model
  top->uart_rx = (*uart)(top->uart_tx, top->rootp->sim_main__DOT__dut__DOT__wb_uart__DOT__wbuart__DOT__uart_setup);

  //Detect and print changes to UART and GPIOs
  if ((uartRxStringPrev != uart->get_rx_string()) ||
      (uartTxStringPrev != uart->get_tx_string())) {

    //Positional printing using ncurses.
    mvprintw(0, 0, "[%lld]", contextp->time());
    mvprintw(1, 0, "UART Out:");
    mvprintw(2, 0, uart->get_rx_string().c_str());
    refresh();

    //Update change detectors
    uartRxStringPrev = uart->get_rx_string();
    uartTxStringPrev = uart->get_tx_string();
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

    //SDL setup
    SDL_Init(SDL_INIT_VIDEO);
    SDL_CreateWindowAndRenderer(SDL_WINDOW_WIDTH, SDL_WINDOW_WIDTH, SDL_WINDOW_ALWAYS_ON_TOP, &sdl_window, &sdl_renderer);
    SDL_SetRenderDrawBlendMode(sdl_renderer,SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(sdl_renderer, 0, 0, 0, 0);
    SDL_RenderClear(sdl_renderer);
    //Rendering directly into SDL's back buffer is not a good idea because double buffering gets in the way.
    //Instead we incrementally render the VGA output onto an SDL texture object and at regular intervals we copy the texture
    //object to SDL's back buffer.
    sdl_display = SDL_CreateTexture(sdl_renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, SDL_WINDOW_WIDTH, SDL_WINDOW_WIDTH);
    SDL_SetRenderTarget(sdl_renderer, sdl_display);

    //Trace file
    if (tracing_enable) {
      top->trace(tfp, 99); //Trace 99 levels deep.
      tfp->open("simx.fst");
    }
    
    //Curses setup
    initscr();
    cbreak();
    noecho();

    jtag_set_bypass(!attach_debugger);

    // Set Vtop's input signals
    top->clk_i = 0;
    top->uart_rx = 0;
    top->rst_ni = !1;
    tick();
    tick();
    tick();
    tick();
    top->rst_ni = !0;
    tick();
    tick();
    tick();
    tick();
  
    mvprintw(27, 0, "Waiting for Vsync...\n\r");
    refresh();

    //Wait for Vsync before starting the rendering
    while (!top->vga_vsync) {
      tick(); //Advance one clock period.
      if (exit_req) {
        cleanup();
        exit(-1);
      }
    }

    mvprintw(27, 0, "Done.\n\r");
    refresh();

    // When not in interactive mode, simulate for 6000000 timeprecision periods
    while (interactive_mode || (contextp->time() < 6000000)) {
        if (exit_req)
          break;

        // Evaluate model
        tick();        
    }
    
    cleanup();

    // End curses.
    endwin();

    // Checks for automated testing.
    int res = 0;
    std::string uartCheckString("VERA Read Back Test successful.");

    if (uartRxStringPrev.find(uartCheckString) == std::string::npos) {
      printf("Test failed\n");
      printf("Expected: %s\n", uartCheckString.c_str());
      printf("Received: %s\n", uartRxStringPrev.c_str());

      res = 1;
    }
    else {
      printf("Test passed.\n");
    }

    return res;
}
