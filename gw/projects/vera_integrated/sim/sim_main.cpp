#include <getopt.h>
#include <fcntl.h>

// For std::unique_ptr
#include <memory>

#include <string>

// SDL for rendering VGA output
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

//SDL objects:
#define SDL_WINDOW_WIDTH 800

SDL_Event sdl_event;
SDL_Renderer *sdl_renderer;
SDL_Window *sdl_window;
SDL_Texture *sdl_display;

int sdl_2x=0/*750*/, sdl_y=0/*523*/;

bool vsync_prev = true;
bool hsync_prev = true;
bool exit_req = false; //Will be set to true when SDL window is closed by user.

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

//A frame counter is used to determine when to start and stop recording the contents of one frame to a file.
unsigned framecount = 0;
FILE *frameFile = 0;

// Legacy function required only so linking works on Cygwin and MSVC++
double sc_time_stamp() { return 0; }

//Clean-up logic.
static void cleanup() {
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

  //Exit if user closes the SDL window.
  if (SDL_PollEvent(&sdl_event) && sdl_event.type == SDL_QUIT)
    exit_req = true;

  //Clear the screen during Vsync. Note that Vsync is active low.
  if (!top->vga_vsync && vsync_prev) {
    SDL_SetRenderDrawColor(sdl_renderer, 0, 0, 0, 0);
    SDL_RenderClear(sdl_renderer);
    sdl_y = 0;
    ++framecount;

    //Start frame recording on frame 2.
    if (framecount == 2)
      frameFile = fopen("frame.bin", "wb");

    //Stop frame recording on frame 3.
    if (framecount == 3) {
      //printf("SIM: Closing frame file...\n");
      fclose(frameFile);
      frameFile = 0;
    }
  }

  //Render to SDL's back buffer at each Hsync. Note that Hsync is active low.
  if (!top->vga_hsync && hsync_prev) {
    SDL_SetRenderTarget(sdl_renderer, NULL);
    SDL_RenderCopy(sdl_renderer, sdl_display, NULL, NULL);
    SDL_RenderPresent(sdl_renderer);
    SDL_SetRenderTarget(sdl_renderer, sdl_display);
    sdl_2x = 0;
    sdl_y++;
  }

  //A pixel buffer for recording to file.
  static Uint8 pixel[4];

  //Convert RGB4:4:4 to RGB8:8:8.
  pixel[0] = (Uint8)(top->vga_r<<4);
  pixel[1] = (Uint8)(top->vga_g<<4);
  pixel[2] = (Uint8)(top->vga_b<<4);
  pixel[3] = 255;

  //Render the VGA rgb output.
  SDL_SetRenderDrawColor(sdl_renderer, pixel[0], pixel[1], pixel[2], pixel[3]);

  //sdl_2x increments at clock rate (50MHz), i.e. twice the pixel clock rate.
  //=> the pixel's x position corresponds to sdl_2x right shifted by 1.
  SDL_RenderDrawPoint(sdl_renderer, sdl_2x>>1, sdl_y);

  if (frameFile)
    fwrite(pixel, 1, 4, frameFile);

  //sdl_2x increments at clock rate (50MHz), i.e. twice the pixel clock rate.
  ++sdl_2x;

  vsync_prev = top->vga_vsync;
  hsync_prev = top->vga_hsync;

  //Feed our model's uart_tx signal and baud rate to the UART co-simulator.
  //and feed the UART co-simulator output to our model
  top->uart_rx = (*uart)(top->uart_tx,
  top->rootp->sim_main__DOT__dut__DOT__boxlambda_soc_inst__DOT__wbuart_inst__DOT__uart_setup);

  //Detect and print changes to UART
  if (!uart->get_rx_string().empty())  {
    printf("%s", uart->get_rx_string().c_str());
    fflush(stdout);
    //Update change detectors
    uartRxStringPrev += uart->get_rx_string();

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
      switch(getopt(argc, argv, "ith")) {
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
      default :
        printf("SIM: \nVmodel Usage:\n");
        printf("SIM: -h: print this help\n");
        printf("SIM: -t: enable tracing.\n");
        printf("SIM: -i: enable interactive mode.\n");
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

    jtag_set_bypass(!attach_debugger);

    top->clk_i = 0;
    top->uart_rx = 0;
    //Take the system out of reset.
    top->rst_ni = 1;

    // When not in interactive mode, simulate for 19000000 timeprecision periods
    while (interactive_mode || (contextp->time() < 19000000)) {
        if (exit_req)
          break;

        // Evaluate model
        tick();
    }

    printf("\n");
    cleanup();

    // Checks for automated testing.
    std::string uartCheckString1("Read Back Tests successful.");

    if (uartRxStringPrev.find(uartCheckString1) == std::string::npos) {
      printf("SIM: did not find expected string: %s\n", uartCheckString1.c_str());
      printf("SIM: Test failed\n");
      return -1;
    }

    /* V<sprite-bank#> indicates a Vsync IRQ, L = Line IRQ, C = collision IRQ. */
    std::string uartCheckString2("LV0LV0(Forcing sprite collision)LV0CL");

    if (uartRxStringPrev.find(uartCheckString2) == std::string::npos) {
      printf("SIM: did not find expected string: %s\n", uartCheckString2.c_str());
      printf("SIM: Test failed\n");
      return -1;
    }

    printf("SIM: Test passed.\n");
    return 0;
}
